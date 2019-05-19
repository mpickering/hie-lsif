{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module LoadHIE where

import GHC
import HieTypes
import Name
import IfaceType
import FastString
import Data.List

import qualified Data.Array as A
import qualified Data.Map as M

import System.Directory
import System.FilePath


type References a = [Reference a]

type RefMap = References PrintedType

type Reference a = (HieAST a, HieTypes.Identifier, IdentifierDetails a)

type Ref = Reference PrintedType

{- Reading a single HIE file -}

generateReferencesList
   :: Foldable f
   => A.Array TypeIndex HieTypeFlat
   -> f (HieAST TypeIndex)
   -> References PrintedType
generateReferencesList ty_array hie = foldr (\ast m -> print_and_go ast ++ m) [] hie
   where
     print_and_go = go . recoverFullIfaceTypes ty_array
     go :: HieAST a -> References a
     go ast = this ++ concatMap go (nodeChildren ast)
       where
         this = map (\(a, b) -> (ast,a, b)) (M.toList (nodeIdentifiers $ nodeInfo ast))

genRefMap :: HieFile -> (FilePath, Module, References PrintedType)
genRefMap hf = (fp, ref_mod, generateReferencesList (hie_types hf) $ getAsts $ (hie_asts hf))
  where
    ref_mod = hie_module hf
    fp  = hie_hs_file hf



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




----
--
-- Copied from haddock




-- * HIE file procesddsing

 -- This belongs in GHC's HieUtils...

 -- | Pretty-printed type, ready to be turned into HTML by @xhtml@
type PrintedType = String

 -- | Expand the flattened HIE AST into one where the types printed out and
-- ready for end-users to look at.
--
-- Using just primitives found in GHC's HIE utilities, we could write this as
-- follows:
--
-- > 'recoverFullIfaceTypes' dflags hieTypes hieAst
-- >     = 'fmap' (\ti -> 'showSDoc' df .
-- >                      'pprIfaceType' $
-- >                      'recoverFullType' ti hieTypes)
-- >       hieAst
--
-- However, this is very inefficient (both in time and space) because the
-- mutliple calls to 'recoverFullType' don't share intermediate results. This
-- function fixes that.
recoverFullIfaceTypes
  :: A.Array TypeIndex HieTypeFlat -- ^ flat types
  -> HieAST TypeIndex              -- ^ flattened AST
  -> HieAST PrintedType       -- ^ full AST
recoverFullIfaceTypes flattened ast = fmap (unflattened A.!) ast
    where

     -- The recursion in 'unflattened' is crucial - it's what gives us sharing
    -- between the IfaceType's produced
    unflattened :: A.Array TypeIndex PrintedType
    unflattened = fmap (\flatTy -> go (fmap (unflattened A.!) flatTy)) flattened

     -- Unfold an 'HieType' whose subterms have already been unfolded
    go :: HieType PrintedType -> PrintedType
    go (HTyVarTy n) = getOccString n
    go (HAppTy a b) = wrap b (a ++ hieToIfaceArgs b)
    go (HLitTy l) = ifaceTyLit l
    go (HForAllTy ((n,_k),_af) t) =
      "forall " ++ getOccString n ++ " . " ++ t
    go (HFunTy a b) = a ++ " -> " ++ b
    go (HQualTy con b) = con ++ " => " ++ b
    go (HCastTy a) = a
    go HCoercionTy = "<co>"
    go (HTyConApp (IfaceTyCon{ifaceTyConName}) xs) =
      wrap xs $ (getOccString ifaceTyConName ++ " " ++ hieToIfaceArgs xs)

    wrap (HieArgs args) =
        case args of
                 [] -> id
                 _  -> \s -> "(" ++ s ++ ")"

    hieToIfaceArgs :: HieArgs PrintedType -> PrintedType
    hieToIfaceArgs (HieArgs args) = go' args
      where
        go' [] = ""
        go' ((True ,x):xs) = " " ++ x ++ go' xs
        go' ((False,x):xs) = " " ++ x ++ go' xs

    ifaceTyLit :: IfaceTyLit -> PrintedType
    ifaceTyLit (IfaceNumTyLit n) = show n
    ifaceTyLit (IfaceStrTyLit fs) = "\"" ++ unpackFS fs ++ "\""
