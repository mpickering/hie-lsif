{-# LANGUAGE TupleSections #-}
module LoadHIE where

import GHC
import OccName
import HieTypes
import HieUtils
import Name
import NameCache
import HieBin
import UniqSupply


import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.IO.Class

import Data.Aeson

import Data.Coerce

data LoadedHIE = LoadedHIE Module RefMap

type References a = [Reference a]

type RefMap = References TypeIndex

type Reference a = (Span, Identifier, IdentifierDetails a)

type Ref = Reference TypeIndex

type ModRefs = [(FilePath, Module, References TypeIndex)]

type DbMonad a = StateT NameCache (WriterT ModRefs IO) a

generateReferencesList
   :: Foldable f
   => f (HieAST a)
   -> References a
generateReferencesList hie = foldr (\ast m -> go ast ++ m) [] hie
   where
     go :: HieAST a -> References a
     go ast = this ++ concatMap go (nodeChildren ast)
       where
         this = map (\(a, b) -> (nodeSpan ast,a, b)) (M.toList (nodeIdentifiers $ nodeInfo ast))

genRefMap :: HieFile -> (FilePath, Module, References TypeIndex)
genRefMap hf = (fp, mod, generateReferencesList $ getAsts $ hie_asts hf)
  where
    mod = hie_module hf
    fp  = hie_hs_file hf

collectReferences :: FilePath -> DbMonad ()
collectReferences path = do
  nc <- get
  (hiefile, nc') <- liftIO $ readHieFile nc path
  put nc'
  tell $ [genRefMap hiefile]

collectAllReferences :: [FilePath] -> IO ModRefs
collectAllReferences xs = do
  uniq_supply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniq_supply []
  execWriterT $ flip evalStateT nc
              $ mapM_ collectReferences xs

-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
  exists <- doesPathExist path
  if exists then do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isFile && ("hie" `isExtensionOf` path) then do
      path' <- makeAbsolute path
      return [path']
    else if isDir then do
      cnts <- listDirectory path
      withCurrentDirectory path $ foldMap getHieFilesIn cnts
    else return []
  else
    return []

data HieDbConf =
  HieDbConf
  { in_dir :: FilePath
  , ofile :: FilePath
  }

{-
interactive :: RefMap -> IO ()
interactive rf = do
  putStrLn "\t 1) Type/Type Class"
  putStrLn "\t 2) Data Constructor"
  putStrLn "\t 3) Var"
  putStr "Choose Name Space(default 3): "
  n <- getLine
  let ns = case reads n of
        (1,_):_ -> tcClsName
        (2,_):_ -> dataName
        _ -> varName
  putStr "Enter module: "
  mod <- getLine
  putStr "Enter name: "
  name <- getLine

  case M.lookup (mkOccName ns name, mkModuleName mod) m of
    Just refs -> printRefs refs
    Nothing -> putStrLn "Not found"

  putStr "q to quit, otherwise continue"
  c <- getLine
  when (c /= "q") $ interactive rf

parseConf :: [String] -> HieDbConf
parseConf [dir,out] = HieDbConf dir out
parseConf ("-d":dir:xs) = (parseConf xs){in_dir = dir}
parseConf ("-o":file:xs) = (parseConf xs){ofile = file}
parseConf (dir:xs) = (parseConf xs){in_dir = dir}
parseConf _ = HieDbConf "." "./out.hiedb"
-}
