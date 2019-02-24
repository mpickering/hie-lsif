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

import LoadHIE

import qualified Streaming.Prelude as St
import qualified System.IO as IO
import Streaming

import qualified Data.ByteString.Lazy.Char8 as L

import NameCache
import HieBin
import UniqSupply

type ModRef = (FilePath, Module, References PrintedType)

type IndexM a = StateT IndexS IO a

type IndexStreamG v a =  Stream (Of v) (StateT IndexS IO) a

type IndexStream a = IndexStreamG Value a

{- Global state of the indexer -}

type ResultSetMap = NameEnv Int
type ReferenceResultMap = NameEnv Int
type RangeMap = M.Map RealSrcSpan Int
type ModuleMap = M.Map Module Int

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
  return (genRefMap hiefile)

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
    do_one_file :: Int -> ModRef -> IndexStream ()
    do_one_file proj_node (fp, ref_mod, r) = do
      dn <- mkDocument proj_node root fp
      St.for (St.each r) (mkReferences dn ref_mod)


mkDocument :: Int -> FilePath -> FilePath -> IndexStream Int
mkDocument proj_node root fp = do
  doc <- mkDocumentNode root fp
  mkContainsEdge proj_node doc
  return doc

emitExports :: Int -> IndexStream ()
emitExports dn = do
  es <- gets exports
  i <- mkExportResult es
  void $ mkExportsEdge dn i


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
mkReferences :: Int -> Module -> Ref -> IndexStream ()
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

prefix :: String
prefix = "file://"

mkDocumentNode :: FilePath -> FilePath -> IndexStream Int
mkDocumentNode root fp =
  let val = [ "label" .= ("document" :: Text)
            , "uri" .= (prefix ++ root ++ fp)
            , "languageId" .= ("haskell" :: Text)
            , "type" .= ("vertex" :: Text) ]
  in uniqueNode val

-- For a given identifier, make the ResultSet node and add the mapping
-- from that identifier to its result set
mkResultSet :: Name -> IndexStream Int
mkResultSet n = do
  m <- gets resultSetMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- mkResultSetWithKey n
      modify (\s -> s { resultSetMap = extendNameEnv m n i } )
      return i


mkResultSetWithKey :: Name -> IndexStream Int
mkResultSetWithKey n =
  "resultSet" `vWith` ["key" .= nameToKey n]


vWith :: Text -> [Pair] -> IndexStream Int
vWith l as = uniqueNode $ as ++  ["type" .= ("vertex" :: Text), "label" .= l ]

vertex :: Text -> IndexStream Int
vertex t = t `vWith` []


mkProjectVertex :: Maybe FilePath -> IndexStream Int
mkProjectVertex mcabal_file =
  "project" `vWith` (["projectFile" .= ("file://" ++ fn) | Just fn <- [mcabal_file]]
                    ++ ["language" .= ("haskell" :: Text)])


mkHover :: Int -> HieAST PrintedType -> IndexStream ()
mkHover range_id node =
  case mkHoverContents node of
    Nothing -> return ()
    Just c  -> do
      hr_id <- mkHoverResult c
      mkHoverEdge range_id hr_id



mkHoverResult :: Value -> IndexStream Int
mkHoverResult c =
  let result = "result" .= (object [ "contents" .= c ])
  in "hoverResult" `vWith` [result]

mkHoverContents :: HieAST PrintedType -> Maybe Value
mkHoverContents Node{nodeInfo} =
  case nodeType nodeInfo of
    [] -> Nothing
    (x:_) -> Just (object ["language" .= ("haskell" :: Text) , "value" .= x])

mkExternalImportResult :: IndexStream Int
mkExternalImportResult = vertex "externalImportResult"


mkExternalImportItem :: Name -> Int -> IndexStream Int
mkExternalImportItem n rid =
  "externalImportItem" `vWith` mkExpImpPairs n rid


mkExportResult :: [(Name, Int)] -> IndexStream Int
mkExportResult es =
  "exportResult" `vWith` ["result" .= (map (uncurry mkExportItem) es)]


mkExpImpPairs :: Name -> Int -> [Pair]
mkExpImpPairs name rid = [ "moniker" .= occNameString (getOccName name)
                         , "rangeIds" .= [rid] ]

mkExportItem :: Name -> Int -> Value
mkExportItem n rid = object (mkExpImpPairs n rid)

-- There are not higher equalities between edges.
mkEdgeWithProp :: Maybe Text -> Int -> Int -> Text -> IndexStream ()
mkEdgeWithProp mp from to l =
  void . uniqueNode $
    [ "type" .= ("edge" :: Text)
    , "label" .= l
    , "outV" .= from
    , "inV"  .= to ] ++ ["property" .= p | Just p <- [mp] ]

mkEdge :: Int -> Int -> Text -> IndexStream ()
mkEdge = mkEdgeWithProp Nothing

mkSpan :: Int -> Int -> Value
mkSpan l c = object ["line" .= l, "character" .= c]

mkRefersTo, mkContainsEdge, mkRefEdge, mkDefEdge, mkExportsEdge
  , mkDefinitionEdge, mkReferencesEdge, mkItemEdge
  , mkImportsEdge, mkHoverEdge :: Int -> Int -> IndexStream ()
mkRefersTo from to = mkEdge from to "refersTo"

mkExportsEdge from to = mkEdge from to "exports"

mkDefEdge from to = mkEdgeWithProp (Just "definition") from to "item"
mkRefEdge from to = mkEdgeWithProp (Just "reference")  from to "item"


mkContainsEdge from to = mkEdge from to "contains"

mkDefinitionEdge from to = mkEdge from to "textDocument/definition"
mkReferencesEdge from to = mkEdge from to "textDocument/references"
mkItemEdge from to = mkEdge from to "item"
mkImportsEdge from to = mkEdge from to "imports"

mkHoverEdge from to = mkEdge from to "textDocument/hover"

mkDefinitionResult :: ToJSON v => v -> IndexStream Int
mkDefinitionResult r =
 "definitionResult" `vWith` ["result" .= r]

mkReferenceResult :: Name -> IndexStream Int
mkReferenceResult n = do
  m <- gets referenceResultMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- vertex "referenceResult"
      modify (\s -> s { referenceResultMap = extendNameEnv m n i } )
      return i


-- LSIF indexes from 0 rather than 1
mkRange :: Span -> IndexStream Int
mkRange s = do
  m <- gets rangeMap
  case M.lookup s m of
    Just i -> return i
    Nothing -> do
      i <- "range" `vWith`
            ["start" .= (mkSpan ls cs), "end" .= (mkSpan le ce)]
      modify (\st -> st { rangeMap = M.insert s i m } )
      return i

  where
    ls = srcSpanStartLine s - 1
    cs = srcSpanStartCol s - 1
    le = srcSpanEndLine s - 1
    ce = srcSpanEndCol s - 1

-- | Make a range and put bind it to the document
mkRangeIn :: Int -> Span -> IndexStream Int
mkRangeIn doc s = do
  v <- mkRange s
  void $ mkContainsEdge doc v
  return v

getId :: IndexStream Int
getId = do
  s <- gets counter
  modify (\st -> st { counter = s + 1 })
  return s

-- Tag a document with a unique ID
uniqueNode :: [Pair] -> IndexStream Int
uniqueNode o = do
  val <- getId
  tellOne (object ("id" .= val : o))
  return val

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
