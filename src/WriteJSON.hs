{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Outputable

type M a = WriterT [Value] (StateT MS IO) a

type Identifier = (OccName, ModuleName)

type ResultSetMap = NameEnv Int
type ReferenceResultMap = NameEnv Int

data MS = MS { counter :: !Int
             , resultSetMap :: ResultSetMap
             , referenceResultMap :: ReferenceResultMap }

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



mkDocumentNode :: FilePath -> M Int
mkDocumentNode fp =
  let val = [ "label" .= ("document" :: Text)
            , "uri" .= fp
            , "languageId" .= ("haskell" :: Text)
            , "type" .= ("vertex" :: Text) ]
  in uniqueNode val

generateJSON :: FilePath -> RefMap -> M ()
generateJSON fp m = do
  dn <- mkDocumentNode fp
  mapM_ (mkReferences dn) m

getValBind :: S.Set ContextInfo -> Maybe ContextInfo
getValBind s = find go (S.toList s)
  where
    go (ValBind {}) = True
    go _ = False


mkReferences :: Int -> Ref -> M ()
mkReferences dn r@(s, Right id, id_details)
  | Just (ValBind bt sc (Just entire_span)) <- getValBind (identInfo id_details) = do
    -- Definition
    rs <- mkResultSet id
    def_range <- mkRangeIn dn s
    def_result <- mkDefinitionResult def_range
    _ <- mkDefinitionEdge rs def_result

    -- Reference
    rr <- mkReferenceResult id
    mkRefersTo def_range rs
    mkDefEdge rr def_range
    mkReferencesEdge rr rs


    liftIO $ print (s, occNameString (getOccName id), (identInfo id_details))
  | Use `S.member` identInfo id_details = do
    use_range <- mkRangeIn dn s
    rs <- mkResultSet id
    rr <- mkReferenceResult id
    mkRefersTo use_range rs
    mkRefEdge rr use_range
    liftIO $ print (s, occNameString (getOccName id), (identInfo id_details))
  | otherwise =
    liftIO $ print (s, occNameString (getOccName id), (identInfo id_details))

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


mkRange :: Span -> M Int
mkRange s =
  uniqueNode $
    "start" .= (mkSpan ls cs) : "end" .= (mkSpan le ce) : vps "range"
  where
    ls = srcSpanStartLine s - 1
    cs = srcSpanStartCol s
    le = srcSpanEndLine s - 1
    ce = srcSpanEndCol s

-- | Make a range and put bind it to the document
mkRangeIn :: Int -> Span -> M Int
mkRangeIn doc s = do
  v <- mkRange s
  mkContainsEdge doc v
  return v



initialState = MS 1 emptyNameEnv emptyNameEnv

writeJSON :: FilePath -> RefMap -> IO ()
writeJSON fp r = do
  ref <- flip evalStateT initialState (execWriterT (generateJSON fp r))
  let res = encode ref
  L.writeFile "test.json" res

