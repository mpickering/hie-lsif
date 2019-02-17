{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module WriteJSON where

import GHC
import OccName
import HieTypes hiding (Identifier)
import HieUtils
import Name
import NameCache
import HieBin
import UniqSupply


import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.List

import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.IO.Class

import NameEnv

import Data.Aeson
import Data.Aeson.Types

import Data.Coerce

import Data.Text(Text)

import LoadHIE

import qualified Data.HashMap.Lazy as H
import qualified Data.Aeson.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Outputable

type M a = WriterT [Value] (StateT MS IO) a

type Identifier = (OccName, ModuleName)

type ResultSetMap = NameEnv Int
type ReferenceResultMap = NameEnv Int
type RangeMap = M.Map RealSrcSpan Int
type ModuleMap = M.Map Module Int

data MS = MS { counter :: !Int
             , resultSetMap :: ResultSetMap
             , referenceResultMap :: ReferenceResultMap
             , rangeMap :: RangeMap
             , exports :: [(Name, Int)]
             , importMap :: ModuleMap }

addExport :: Name -> Int -> M ()
addExport n rid = modify (\st -> st { exports = (n, rid) : exports st })

getId :: M Int
getId = do
  s <- gets counter
  modify (\st -> st { counter = s + 1 })
  return s

-- Tag a document with a unique ID
uniqueNode :: [Pair] -> M Int
uniqueNode o = do
  val <- getId
  tellOne (object ("id" .= val : o))
  return val

tellOne x = tell [x]

root = "/Users/matt/hie-lsif/test/simple-tests/"

prefix = "file://"

mkDocumentNode :: FilePath -> M Int
mkDocumentNode fp =
  let val = [ "label" .= ("document" :: Text)
            , "uri" .= (prefix ++ root ++ fp)
            , "languageId" .= ("haskell" :: Text)
            , "type" .= ("vertex" :: Text) ]
  in uniqueNode val

{-
addImportedReference :: Int -> Module -> Name -> M ()
addImportedReference use_range m n = do
  im_r <- addImportedModule m
  eii <- mkExternalImportItem n use_range
  mkItemEdge im_r eii
  return ()
  -}



{-
addImportedModule :: Module -> M Int
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


generateJSON :: ModRefs -> M ()
generateJSON m = do
  rs <- mapM (\(fp, m, r) -> (,m, r) <$> mkDocumentNode fp ) m
  mapM_ do_one_file rs

  --emitExports dn
  where
    do_one_file (dn, m, r) = mapM_ (mkReferences dn m) r

emitExports :: Int -> M ()
emitExports dn = do
  es <- gets exports
  i <- mkExportResult es
  void $ mkExportsEdge dn i



getBind :: S.Set ContextInfo -> Maybe ContextInfo
getBind s = msum $ map go (S.toList s)
  where
    go c = case c of
             ValBind {} -> Just c
             PatternBind {} -> Just c
             _ -> Nothing


mkReferences :: Int -> Module -> Ref -> M ()
mkReferences dn _ r@(ast, Right id, id_details)
  | Just b <- getBind (identInfo id_details) = do
    -- Definition
    let s = nodeSpan ast
    rs <- mkResultSet id
    def_range <- mkRangeIn dn s
    def_result <- mkDefinitionResult def_range
    _ <- mkDefinitionEdge rs def_result

    -- Reference
    rr <- mkReferenceResult id
    mkRefersTo def_range rs
    mkDefEdge rr def_range
    mkReferencesEdge rs rr

    -- Export
    --addExport id def_range


    liftIO $ print (s, occNameString (getOccName id), (identInfo id_details))
  | Use `S.member` identInfo id_details = do
    let s = nodeSpan ast
    use_range <- mkRangeIn dn s
    rs <- mkResultSet id
    rr <- mkReferenceResult id
    mkRefersTo use_range rs
    mkRefEdge rr use_range
    mkHover use_range ast

{-
    case nameModule_maybe id of
      Just m | not (isGoodSrcSpan (nameSrcSpan id))  -> void $ addImportedReference use_range m id
      _ -> return ()
      -}
    liftIO $ print (s, nameStableString id, nameSrcSpan id, occNameString (getOccName id), (identInfo id_details))
  | otherwise =
    liftIO $ print (nodeSpan ast, occNameString (getOccName id), (identInfo id_details))
mkReferences dn m (s, Left mn, id_detail)  =
  liftIO $ print (nodeSpan s , moduleNameString mn)

nameToKey :: Name -> String
nameToKey n = occNameString (getOccName n)

-- For a given identifier, make the ResultSet node and add the mapping
-- from that identifier to its result set
mkResultSet :: Name -> M Int
mkResultSet n = do
  m <- gets resultSetMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- uniqueNode $ ("key"  .= nameToKey n)
                   : vps "resultSet"
      modify (\s -> s { resultSetMap = extendNameEnv m n i } )
      return i


vps :: Text -> [Pair]
vps l = ["type" .= ("vertex" :: Text), "label" .= l ]

mkHover :: Int -> HieAST PrintedType -> M ()
mkHover range_id node =
  case mkHoverContents node of
    Nothing -> return ()
    Just c  -> do
      hr_id <- mkHoverResult c
      void $ mkHoverEdge range_id hr_id


mkHoverEdge :: Int -> Int -> M Int
mkHoverEdge from to = mkEdge from to "textDocument/hover"

mkHoverResult :: Value -> M Int
mkHoverResult c =
  let result = "result" .= (object [ "contents" .= c ])
  in uniqueNode $ result : vps "hoverResult"

mkHoverContents :: HieAST PrintedType -> Maybe Value
mkHoverContents Node{nodeInfo} =
  case nodeType nodeInfo of
    [] -> Nothing
    (x:_) -> Just (object ["language" .= ("haskell" :: Text) , "value" .= x])

mkExternalImportResult :: M Int
mkExternalImportResult = uniqueNode $ vps "externalImportResult"

mkImportsEdge from to = mkEdge from to "imports"

mkExternalImportItem :: Name -> Int -> M Int
mkExternalImportItem n rid = uniqueNode $ (vps "externalImportItem"
                                          ++ (mkExpImpPairs n rid))

mkItemEdge from to = mkEdge from to "item"


mkExportResult :: [(Name, Int)] -> M Int
mkExportResult es =
  uniqueNode $ "result" .= (map (uncurry mkExportItem) es)
                      : vps "exportResult"


mkExpImpPairs name rid = [ "moniker" .= occNameString (getOccName name)
                               , "rangeIds" .= [rid] ]

mkExportItem :: Name -> Int -> Value
mkExportItem n rid = object (mkExpImpPairs n rid)

mkEdgeWithProp :: Maybe Text -> Int -> Int -> Text -> M Int
mkEdgeWithProp mp from to l =
  uniqueNode $
    [ "type" .= ("edge" :: Text)
    , "label" .= l
    , "outV" .= from
    , "inV"  .= to ] ++ ["property" .= p | Just p <- [mp] ]

mkEdge :: Int -> Int -> Text -> M Int
mkEdge = mkEdgeWithProp Nothing

mkSpan :: Int -> Int -> Value
mkSpan l c = object ["line" .= l, "character" .= c]

mkRefersTo :: Int -> Int -> M Int
mkRefersTo from to = mkEdge from to "refersTo"

mkExportsEdge from to = mkEdge from to "exports"

mkDefEdge from to = mkEdgeWithProp (Just "definition") from to "item"
mkRefEdge from to = mkEdgeWithProp (Just "reference")  from to "item"

mkContainsEdge from to = mkEdge from to "contains"

mkDefinitionResult r = uniqueNode $ "result" .= r : vps "definitionResult"

mkDefinitionEdge from to = mkEdge from to "textDocument/definition"
mkReferencesEdge from to = mkEdge from to "textDocument/references"

mkReferenceResult :: Name -> M Int
mkReferenceResult n = do
  m <- gets referenceResultMap
  case lookupNameEnv m n of
    Just i  -> return i
    Nothing -> do
      i <- uniqueNode (vps "referenceResult")
      modify (\s -> s { referenceResultMap = extendNameEnv m n i } )
      return i


-- LSIF indexes from 0 rather than 1
mkRange :: Span -> M Int
mkRange s = do
  m <- gets rangeMap
  case M.lookup s m of
    Just i -> return i
    Nothing -> do
      i <- uniqueNode $
            "start" .= (mkSpan ls cs) : "end" .= (mkSpan le ce) : vps "range"
      modify (\st -> st { rangeMap = M.insert s i m } )
      return i

  where
    ls = srcSpanStartLine s - 1
    cs = srcSpanStartCol s - 1
    le = srcSpanEndLine s - 1
    ce = srcSpanEndCol s - 1

-- | Make a range and put bind it to the document
mkRangeIn :: Int -> Span -> M Int
mkRangeIn doc s = do
  v <- mkRange s
  mkContainsEdge doc v
  return v



initialState = MS 1 emptyNameEnv emptyNameEnv M.empty [] M.empty

writeJSON :: FilePath -> ModRefs -> IO ()
writeJSON fp r = do
  ref <- flip evalStateT initialState (execWriterT (generateJSON r))
  let res = encode ref
  L.writeFile "test.json" res


