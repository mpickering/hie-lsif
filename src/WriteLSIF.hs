{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Data.Aeson
import Data.Aeson.Types

import Data.Text(Text)
import qualified Data.Text as T

import System.FilePath

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
import LSIF (LsifId)

type ModRef = (FilePath, Module, References PrintedType)

type IndexM a = StateT IndexS IO a

type IndexStreamG v a =  Stream (Of v) (StateT IndexS IO) a

type IndexStream a = IndexStreamG Value a

{- Global state of the indexer -}

type ResultSetMap = NameEnv LsifId
type ReferenceResultMap = NameEnv LsifId
type RangeMap = M.Map RealSrcSpan LsifId
type ModuleMap = M.Map Module LsifId

data IndexS = IndexS
             { counter :: !Int
             , nameCache :: NameCache
             , resultSetMap :: ResultSetMap
             , referenceResultMap :: ReferenceResultMap
             , rangeMap :: RangeMap
             -- Unused at the moment
             , exports :: [(Name, Int)]
             , importMap :: ModuleMap }

initialState :: IO IndexS
initialState = do
  uniq_supply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniq_supply []
  return $ IndexS 1 nc emptyNameEnv emptyNameEnv M.empty [] M.empty

addExport :: Name -> Int -> IndexStream ()
addExport n rid = modify (\st -> st { exports = (n, rid) : exports st })

{- Top level entry point -}

generateFromDir :: FilePath -> FilePath -> IO ()
generateFromDir root hie_dir =
  getHieFilesIn hie_dir >>= writeJSON root . collectAllReferences

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

writeJSON :: FilePath -> IndexStreamG ModRef () -> IO ()
writeJSON root r = do
  writeLSIF (generateJSON root r)

-- LSIF needs to write an array but we have a stream of objects so
-- we have to fake the list part, it's really annoying.
writeLSIF :: Stream (Of Value) (StateT IndexS IO) r -> IO r
writeLSIF s = do
  is <- initialState
  h <- IO.openFile "test.json" IO.WriteMode
  IO.hPutChar h '['
  r <- flip evalStateT is . St.toHandle h . St.intersperse "," . St.map (L.unpack . encode) $ s
  IO.hPutChar h ']'
  IO.hClose h
  return r

{- Start making the aeson values to output -}

generateJSON :: FilePath -> IndexStreamG ModRef () -> IndexStream ()
generateJSON root m = do
  proj_node <- mkProjectVertex Nothing
  (St.for m (do_one_file proj_node))
  where
    do_one_file :: LsifId -> ModRef -> IndexStream ()
    do_one_file proj_node (fp, ref_mod, r) = do
      dn <- mkDocument proj_node root fp
      St.for (St.each r) (mkReferences dn ref_mod)


mkDocument :: LsifId -> FilePath -> FilePath -> IndexStream LsifId
mkDocument proj_node root fp = do
  doc <- mkDocumentNode root fp
  mkContainsEdge proj_node doc
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
mkReferences :: LsifId -> Module -> Ref -> IndexStream ()
mkReferences dn _ (ast, Right ref_id, id_details)
  | Just {} <- getBind (identInfo id_details) = do
    -- Definition
    let s = nodeSpan ast
    rs <- mkResultSet ref_id
    def_range <- mkRangeIn dn s
    def_result <- mkDefinitionResult def_range
    _ <- mkDefinitionEdge rs def_result

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

mkDocumentNode :: FilePath -> FilePath -> IndexStream LsifId
mkDocumentNode root fp =
  let uri = (LSP.filePathToUri $ root </> fp)
      languageId = "haskell"
  in uniqueNode $ \i -> LSIF.mkDocument i uri languageId Nothing Nothing

-- For a given identifier, make the ResultSet node and add the mapping
-- from that identifier to its result set
mkResultSet :: Name -> IndexStream LsifId
mkResultSet n = do
  m <- gets resultSetMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- mkResultSetWithKey n
      modify (\s -> s { resultSetMap = extendNameEnv m n i } )
      return i


mkResultSetWithKey :: Name -> IndexStream LsifId
mkResultSetWithKey n = uniqueNode LSIF.mkResultSet
  -- "resultSet" `vWith` ["key" .= nameToKey n]


mkProjectVertex :: Maybe FilePath -> IndexStream LsifId
mkProjectVertex mcabal_file =
    uniqueNode $ \i -> LSIF.mkProject i kind resource Nothing Nothing
  where
    kind = "haskell"
    resource = LSP.filePathToUri <$> mcabal_file


mkHover :: LsifId -> HieAST PrintedType -> IndexStream ()
mkHover range_id node =
  case mkHoverContents node of
    Nothing -> return ()
    Just c  -> do
      hr_id <- mkHoverResult c
      void $ uniqueNode $ \i ->
        LSIF.mkHoverEdge i range_id hr_id

mkHoverResult :: LSP.HoverContents -> IndexStream LsifId
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

-- There are not higher equalities between edges.
mkItemEdge :: LsifId -> LsifId -> LSIF.ItemEdgeProperties -> IndexStream ()
mkItemEdge from to prop = void . uniqueNode $ \i ->
  LSIF.mkItemEdge i from to prop

mkRefersTo, mkContainsEdge, mkRefEdge, mkDefEdge, mkDefinitionEdge
  , mkReferencesEdge, mkHoverEdge :: LsifId -> LsifId -> IndexStream ()
mkRefersTo from to = void . uniqueNode $ \i ->
  LSIF.mkRefersToEdge i from to
mkContainsEdge from to = void . uniqueNode $ \i ->
  LSIF.mkContainsEdge i from to
mkDefinitionEdge from to = void . uniqueNode $ \i ->
  LSIF.mkDefinitionEdge i from to
mkReferencesEdge from to = void . uniqueNode $ \i ->
  LSIF.mkReferencesEdge i from to
mkHoverEdge from to = void . uniqueNode $ \i ->
  LSIF.mkHoverEdge i from to

mkDefEdge from to = mkItemEdge from to LSIF.Definitions
mkRefEdge from to = mkItemEdge from to LSIF.References

mkDefinitionResult :: LsifId -> IndexStream LsifId
mkDefinitionResult r = uniqueNode $ \i ->
 LSIF.mkDefinitionResult i (Just [LSIF.InL r])

mkReferenceResult :: Name -> IndexStream LsifId
mkReferenceResult n = do
  m <- gets referenceResultMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- uniqueNode $ \i -> LSIF.mkReferenceResult i Nothing Nothing Nothing Nothing
      modify (\s -> s { referenceResultMap = extendNameEnv m n i } )
      return i

-- LSIF indexes from 0 rather than 1
mkRange :: Span -> IndexStream LsifId
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
mkRangeIn :: LsifId -> Span -> IndexStream LsifId
mkRangeIn doc s = do
  v <- mkRange s
  void $ mkContainsEdge doc v
  return v

getId :: IndexStream LsifId
getId = do
  s <- gets counter
  modify (\st -> st { counter = s + 1 })
  return (LSIF.InL s)

-- Tag a document with a unique ID
uniqueNode :: ToJSON a => (LSIF.LsifId -> a) -> IndexStream LsifId
uniqueNode k = do
  i <- getId
  tellOne $ toJSON $ k i
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
