{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{- Generate an LSIF file from an HIE file -}
module WriteLSIF where

import GHC
import OccName
import HieTypes hiding (Identifier)
import Name


import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.State.Strict
import NameEnv

import Data.Aeson (ToJSON, encode)

import Data.Foldable

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString            ( ByteString )
import qualified Data.ByteString.Base64 as B64

import System.FilePath
import System.Directory

import LoadHIE

import qualified Streaming.Prelude as St
import qualified System.IO as IO
import Streaming

import qualified Data.ByteString.Lazy.Char8 as L

import NameCache
import HieBin
import UniqSupply

import qualified Language.Haskell.LSP.Types                 as LSP
import qualified LSIF
import LSIF (LsifId, ElementId, Element)
import LSIF (RangeId, DefinitionResultId, ReferenceResultId, HoverResultId, ProjectId
            , ResultSetId, DocumentId)

type ModRef = (FilePath, Module, References PrintedType, ByteString)

type IndexM a = StateT IndexS IO a

type IndexStreamG v a =  Stream (Of v) (StateT IndexS IO) a

type IndexStream a = IndexStreamG L.ByteString a

{- Global state of the indexer -}

type ResultSetMap = NameEnv LSIF.ResultSetId
type ReferenceResultMap = NameEnv LSIF.ReferenceResultId
type RangeMap = M.Map RealSrcSpan LSIF.RangeId
-- type ModuleMap = M.Map Module (LsifId SomeId)

data IndexS = IndexS
             { counter :: !Int
             , nameCache :: NameCache
             , resultSetMap :: ResultSetMap
             , referenceResultMap :: ReferenceResultMap
             , rangeMap :: RangeMap
             -- Unused at the moment
             , exports :: [(Name, Int)]
             }

data Opts
  = Opts
  { root_dir :: FilePath
  , hie_dir :: FilePath
  , include_contents :: Bool
  }

initialState :: IO IndexS
initialState = do
  uniq_supply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniq_supply []
  return $ IndexS 1 nc emptyNameEnv emptyNameEnv M.empty []

addExport :: Name -> Int -> IndexStream ()
addExport n rid = modify (\st -> st { exports = (n, rid) : exports st })

{- Top level entry point -}

generateFromDir :: Opts -> IO ()
generateFromDir opts  =
  getHieFilesIn (hie_dir opts) >>= writeJSON (root_dir opts) (include_contents opts) . collectAllReferences

{- Read files into the stream, this ensures only one HIE file is in memory at once -}

collectAllReferences :: [FilePath] -> IndexStreamG ModRef ()
collectAllReferences xs = St.mapM collectReferences (St.each xs)

collectReferences :: FilePath -> IndexM ModRef
collectReferences path = do
  nc <- gets nameCache
  (hiefile, nc') <- liftIO $ readHieFile nc path
  modify (\s -> s { nameCache = nc' })
  return (genRefMap $ hie_file_result hiefile)

{- Convert an HIE file into a LSIF file -}

writeJSON :: FilePath -> Bool -> IndexStreamG ModRef () -> IO ()
writeJSON root inc r = do
  writeLSIF (generateJSON root inc r)

-- LSIF needs to write an array but we have a stream of objects so
-- we have to fake the list part, it's really annoying.
writeLSIF :: Stream (Of L.ByteString) (StateT IndexS IO) r -> IO r
writeLSIF s = do
  is <- initialState
  h <- IO.openFile "test.json" IO.WriteMode
  IO.hPutChar h '['
  r <- flip evalStateT is . St.toHandle h . St.intersperse "," . St.map L.unpack $ s
  IO.hPutChar h ']'
  IO.hClose h
  return r

{- Start making the aeson values to output -}

findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile root = do
  files <- listDirectory root
  return $ find ("cabal" `isExtensionOf`) files

generateJSON :: FilePath -> Bool -> IndexStreamG ModRef () -> IndexStream ()
generateJSON root inc m = do
  cabal_file <- liftIO $ findCabalFile root
  proj_node <- mkProjectVertex cabal_file
  (St.for m (do_one_file proj_node))
  where
    do_one_file :: ProjectId -> ModRef -> IndexStream ()
    do_one_file proj_node (fp, ref_mod, r, contents) = do
      let content' =
            if inc
            then Just $ T.decodeUtf8 $ B64.encode contents
            else Nothing
      dn <- mkDocument proj_node root fp content'
      St.for (St.each r) (mkReferences dn ref_mod)


mkDocument :: ProjectId -> FilePath -> FilePath -> Maybe Text -> IndexStream DocumentId
mkDocument proj_node root fp content = do
  doc <- mkDocumentNode root fp content
  mkContainsProjDocEdge proj_node doc
  return doc

-- Decide whether a reference is a bind or not.
getBind :: S.Set ContextInfo -> Maybe ContextInfo
getBind s = msum $ map go (S.toList s)
  where
    go c = case c of
             ValBind {} -> Just c
             PatternBind {} -> Just c
             TyVarBind {}   -> Just c
             ClassTyDecl {} -> Just c
             Decl {}        -> Just c
             _ -> Nothing

-- Main function which makes the references/definition entries and emits them
mkReferences :: DocumentId -> Module -> Ref -> IndexStream ()
mkReferences dn _ (ast, Right ref_id, id_details)
  | Just {} <- getBind (identInfo id_details) = do
    -- Definition
    let s = nodeSpan ast
    rs <- mkResultSet ref_id
    def_range <- mkRangeIn dn s
    def_result <- mkDefinitionResult def_range
    mkDefinitionEdge rs def_result

    -- Reference
    rr <- mkReferenceResult ref_id
    mkRefersTo def_range rs
    mkDefEdge rr def_range
    mkReferencesEdge rs rr

    -- Export
    --addExport id def_range


    --liftIO $ print (s, occNameString (getOccName ref_id), (identInfo id_details))
  | Use `S.member` identInfo id_details = do
    let s = nodeSpan ast
    use_range <- mkRangeIn dn s
    rs <- mkResultSet ref_id
    rr <- mkReferenceResult ref_id
    mkRefersTo use_range rs
    mkRefEdge rr use_range
    mkHover use_range ast

{-
    case nameModule_maybe id of
      Just m | not (isGoodSrcSpan (nameSrcSpan id))  -> void $ addImportedReference use_range m id
      _ -> return ()
      -}
    --liftIO $ print (s, nameStableString ref_id, nameSrcSpan ref_id, occNameString (getOccName ref_id), (identInfo id_details))
  | otherwise =
    liftIO $ print (nodeSpan ast, occNameString (getOccName ref_id), (identInfo id_details))
mkReferences _ _ (s, Left mn, _)  =
  liftIO $ print (nodeSpan s , moduleNameString mn)


-- JSON generation functions
--

nameToKey :: Name -> String
nameToKey = getOccString

mkDocumentNode :: FilePath -> FilePath -> Maybe Text -> IndexStream DocumentId
mkDocumentNode root fp content =
  let uri = (LSP.filePathToUri $ root </> fp)
      languageId = "haskell"
  in uniqueNode $ \i -> LSIF.mkDocument i uri languageId Nothing content

-- For a given identifier, make the ResultSet node and add the mapping
-- from that identifier to its result set
mkResultSet :: Name -> IndexStream ResultSetId
mkResultSet n = do
  m <- gets resultSetMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- mkResultSetWithKey n
      modify (\s -> s { resultSetMap = extendNameEnv m n i } )
      return i


mkResultSetWithKey :: Name -> IndexStream ResultSetId
mkResultSetWithKey _n = uniqueNode LSIF.mkResultSet
  -- "resultSet" `vWith` ["key" .= nameToKey n]


mkProjectVertex :: Maybe FilePath -> IndexStream ProjectId
mkProjectVertex mcabal_file =
    uniqueNode $ \i -> LSIF.mkProject i kind resource Nothing Nothing
  where
    kind = "haskell"
    resource = LSP.filePathToUri <$> mcabal_file


mkHover :: RangeId -> HieAST PrintedType -> IndexStream ()
mkHover range_id node =
  case mkHoverContents node of
    Nothing -> return ()
    Just c  -> do
      hr_id <- mkHoverResult c
      mkHoverEdge range_id hr_id

mkHoverResult :: LSP.HoverContents -> IndexStream HoverResultId
mkHoverResult c =
  uniqueNode $ \i -> LSIF.mkHoverResult i (LSP.Hover c Nothing)

mkHoverContents :: HieAST PrintedType -> Maybe LSP.HoverContents
mkHoverContents Node{nodeInfo} =
  case nodeType nodeInfo of
    [] -> Nothing
    xs -> let content = T.unlines $ do
                x <- xs
                ["```haskell",T.pack x,"```"]
          in Just $ LSP.HoverContents $ LSP.MarkupContent LSP.MkMarkdown content

-- There are no higher equalities between edges, so we don't need to return EdgeIds
mkRefersTo :: RangeId -> ResultSetId -> IndexStream ()
mkRefersTo from to = void . uniqueNode $ \i ->
  LSIF.mkRefersToEdge i from to

mkContainsProjDocEdge :: ProjectId -> DocumentId -> IndexStream ()
mkContainsProjDocEdge from to = void . uniqueNode $ \i ->
  LSIF.mkContainsProjDocEdge i from to

mkContainsDocRangeEdge :: DocumentId -> RangeId -> IndexStream ()
mkContainsDocRangeEdge from to = void . uniqueNode $ \i ->
  LSIF.mkContainsDocRangeEdge i from to

mkDefinitionEdge :: ResultSetId -> DefinitionResultId -> IndexStream ()
mkDefinitionEdge from to = void . uniqueNode $ \i ->
  LSIF.mkDefinitionResultEdge i from to

mkReferencesEdge :: ResultSetId -> ReferenceResultId -> IndexStream ()
mkReferencesEdge from to = void . uniqueNode $ \i ->
  LSIF.mkReferencesResultEdge i from to

mkHoverEdge :: RangeId -> HoverResultId -> IndexStream ()
mkHoverEdge from to = void . uniqueNode $ \i ->
  LSIF.mkHoverRangeEdge i from to

mkDefEdge :: ReferenceResultId -> RangeId -> IndexStream ()
mkDefEdge from to = void . uniqueNode $ \i ->
  LSIF.mkItemRefRangeEdge i from to (Just LSIF.Definitions)

mkRefEdge :: ReferenceResultId -> RangeId -> IndexStream ()
mkRefEdge from to = void . uniqueNode $ \i ->
  LSIF.mkItemRefRangeEdge i from to (Just LSIF.References)

mkDefinitionResult :: RangeId -> IndexStream DefinitionResultId
mkDefinitionResult r = uniqueNode $ \i ->
 LSIF.mkDefinitionResult i (Just [LSIF.InL r])

mkReferenceResult :: Name -> IndexStream ReferenceResultId
mkReferenceResult n = do
  m <- gets referenceResultMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- uniqueNode $ \i -> LSIF.mkReferenceResult i Nothing Nothing Nothing Nothing
      modify (\s -> s { referenceResultMap = extendNameEnv m n i } )
      return i

-- LSIF indexes from 0 rather than 1
mkRange :: Span -> IndexStream RangeId
mkRange s = do
  m <- gets rangeMap
  case M.lookup s m of
    Just i -> return i
    Nothing -> do
      i <- uniqueNode $ \i -> LSIF.mkUntaggedRange i (LSP.Position ls cs) (LSP.Position le ce)
      modify (\st -> st { rangeMap = M.insert s i m } )
      return i

  where
    ls = srcSpanStartLine s - 1
    cs = srcSpanStartCol s - 1
    le = srcSpanEndLine s - 1
    ce = srcSpanEndCol s - 1

-- | Make a range and put bind it to the document
mkRangeIn :: DocumentId -> Span -> IndexStream RangeId
mkRangeIn doc s = do
  v <- mkRange s
  void $ mkContainsDocRangeEdge doc v
  return v

getId :: IndexStream (LsifId a)
getId = do
  s <- gets counter
  modify (\st -> st { counter = s + 1 })
  return (LSIF.IdInt s)

-- Tag a document with a unique ID
uniqueNode :: ToJSON (Element t)
           => (LsifId (ElementId t) -> LSIF.Element t)
           -> IndexStream (LsifId (ElementId t))
uniqueNode k = do
  i <- getId
  tellOne $ encode $ k i
  return i

tellOne :: a -> Stream (Of a) (StateT IndexS IO) ()
tellOne x = St.yield x

{-
addImportedReference :: Int -> Module -> Name -> IndexStream ()
addImportedReference use_range m n = do
  im_r <- addImportedModule m
  eii <- mkExternalImportItem n use_range
  mkItemEdge im_r eii
  return ()
  -}

{-
addImportedModule :: Module -> IndexStream Int
addImportedModule m = do
  ma <- gets importMap
  case M.lookup m ma of
    Just i  -> return i
    Nothing -> do
      liftIO $ print (moduleNameString (moduleName m))
      i <- mkDocumentNode "/home/matt/hie-lsif/test/simple-tests/B.js"
      ei <- mkExternalImportResult
      mkImportsEdge i ei

      modify (\s -> s { importMap = M.insert m ei ma } )
      return i
      -}
