{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GADTs                      #-}

module LSIF where

import           Control.Applicative
import qualified Data.Aeson                                 as A
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import qualified Language.Haskell.LSP.Types                 as LSP
import Data.Kind
import qualified Data.Void as Void
import Data.Char
import GHC.Generics hiding (MetaData)
import Data.List
import Unsafe.Coerce
import Data.Proxy

{-
Hacks to get avoid boilerplate when constructing elements.
-}

type Constr a = Args (Rep a) a
-- ^ Type of the constructor for A without `Empty`s
-- For example, given a type
-- data X = X Empty Empty Int Empty Bool Empty String,
-- Constr X = Int -> Bool -> String -> X

-- | Args is used to compute Constr by matching
-- on the generic representation (Rep) of a type
type family Args a r where
  Args (M1 k m c1) r = Args c1 r
  Args (a :*: b) r = Args a (Args b r)
  Args (K1 m Empty) r = r
  Args (K1 m a) r = a -> r
  Args U1 r = r

-- | Typeclass to compute Constr/Args by traversing the generic representation
-- of a type, and filling in `Nothing`s(Unit constructors) wherever an
-- `Empty` type is accepted by the constructor
class MkConstr f where
  -- | Accepts a continuation that we can apply to the final result type.
  -- This allows us to build up our modified constructor recursively, as
  -- well as allowing us to turn the final `Rep` result back into
  -- our original type in mkConstr
  mkConstr' :: forall r a. (f a -> r) -> Args f r

instance MkConstr c1 => MkConstr (M1 k m c1) where
  mkConstr' k = mkConstr' (k . M1)

instance (MkConstr a, MkConstr b) => MkConstr (a :*: b) where
  mkConstr' k = mkConstr' (\x -> mkConstr' (k . (\y -> x :*: y)))

-- If a given field is `Empty`, fill it in with its sole constructor:
-- `Nothing`
instance MkConstr (K1 m Empty) where
  mkConstr' k = k (K1 Nothing)

instance {-# OVERLAPPABLE #-} MkConstr (K1 @k m a) where
  mkConstr' k = coerce $ k . K1
    where
      coerce :: (a -> r) -> Args (K1 @k m a) r
      coerce = unsafeCoerce
      -- This is safe as the only reason GHC is not able
      -- to reduce `Args (K1 m a) r` to `a -> r` is because
      -- of the overlapping type pattern:
      -- `Args (K1 m Empty) r = r`
      -- However, when `(a ~ Empty)`, the overlapping instance above
      -- for `MkConstr (K1 m Empty)` will be selected instead

instance MkConstr U1 where
  mkConstr' k = k U1

-- | Given a type as a Proxy argument, compute its Constr
mkConstr :: forall r. (MkConstr (Rep r), Generic r) => Proxy r -> Constr r
mkConstr _ = mkConstr' (to :: Rep r x -> r)

{-
Actual definitions
-}

aesonOpts :: A.Options
aesonOpts
  = A.defaultOptions
  { A.allNullaryToStringTag = True
  , A.constructorTagModifier = \xs -> map toLower (take 1 xs) ++ drop 1 xs
  , A.omitNothingFields = True
  , A.fieldLabelModifier = drop 1
  }

-- | Empty ~= ()
-- However, we use this instead of () as Aeson has an option
-- to omit Nothing fields, so when we try to encode a type
-- X { foo :: Empty, bar :: Int, baz :: Empty },
-- the resultant JSON doesn't contain spurious fields for
-- foo and baz, just a single field for bar
type Empty = Maybe Void.Void

-- | Untagged union type - used for aeson instances
data (:|:) a b = InL a | InR b deriving (Eq, Show, Ord)

instance (A.ToJSON a, A.ToJSON b) => A.ToJSON (a :|: b) where
  toJSON (InL a) = A.toJSON a
  toJSON (InR b) = A.toJSON b

data IdType
  = RangeId
  | VertexId VertexLabel
  | EdgeId

-- | An Id to identify a vertex or an edge.
data LsifId (a :: IdType) = IdInt Int | IdText Text
  deriving (Eq,Ord,Show)

instance A.ToJSON (LsifId a) where
  toJSON (IdInt a) = A.toJSON a
  toJSON (IdText b) = A.toJSON b

type family ElementId (l :: ElementType) :: IdType where
  ElementId (Edge l) = EdgeId
  ElementId (Vertex ('Range a)) = 'RangeId
  ElementId (Vertex l) = VertexId l

-- | Takes a list of VertexLabels paired with the type of their fields,
-- and an ElementType, and returns the associated type of `t` if the
-- ElementType is `Vertex t`.
-- Similar to the value level `lookup` function in Prelude
type family MatchV (l :: ElementType) (xs :: [(VertexLabel, Type)]) where
  MatchV (Vertex l) ('(l,t) ': xs) = t
  MatchV (Vertex l) ('(k,t) ': xs) = MatchV (Vertex l) xs
  MatchV a b = Empty

-- | 'Match' specialised to a single type
type WhenV t l a = MatchV t '[ '(l,a) ]
-- | 'Match' specialised for the case where a bunch of types
-- have the same associated type
type WhenVs t xs a = MatchV t (Assoc xs a)

-- | Helper for `WhenVs`: Pair each of a list of
-- types with the same associated type
type family Assoc (xs :: [a]) (y :: b) :: [(a,b)] where
  Assoc '[] y = '[]
  Assoc (x ': xs) y = '(x,y) ': (Assoc xs y)

type family RangeOrResultT (a :: RangeOrResult) where
  RangeOrResultT RangeEdge = RangeId
  RangeOrResultT ResultEdge = ResultSetId

-- | Given type when the ElementType is an Edge, Empty otherwise
type family OutE (t :: ElementType) where
  OutE (Edge ('Contains ProjDocContains)) = ProjectId
  OutE (Edge ('Contains DocRangeContains)) = DocumentId
  OutE (Edge ('Item RefRangeItem)) = ReferenceResultId
  OutE (Edge ('Item RefRefItem)) = ReferenceResultId
  OutE (Edge ('Item ImpRangeItem)) = ImplementationResultId
  OutE (Edge ('Item ImpImpItem)) = ImplementationResultId
  OutE (Edge 'RefersTo) = RangeId
  OutE (Edge 'EMoniker) = RangeId
  OutE (Edge 'EPackageInformation) = MonikerId
  OutE (Edge 'TextDocument_DocumentSymbol) = DocumentId
  OutE (Edge 'TextDocument_FoldingRange) = DocumentId
  OutE (Edge 'TextDocument_DocumentLink) = DocumentId
  OutE (Edge ('TextDocument_Diagnostic ProjDiag)) = ProjectId
  OutE (Edge ('TextDocument_Diagnostic ProjDiag)) = DocumentId
  OutE (Edge ('TextDocument_Definition a)) = RangeOrResultT a
  OutE (Edge ('TextDocument_Declaration a)) = RangeOrResultT a
  OutE (Edge ('TextDocument_TypeDefinition a)) = RangeOrResultT a
  OutE (Edge ('TextDocument_Hover a)) = RangeOrResultT a
  OutE (Edge ('TextDocument_References a)) = RangeOrResultT a
  OutE (Edge ('TextDocument_Implementation a)) = RangeOrResultT a
  OutE a = Empty

type family InE (t :: ElementType) where
  InE (Edge ('Contains ProjDocContains)) = DocumentId
  InE (Edge ('Contains DocRangeContains)) = RangeId
  InE (Edge ('Item RefRangeItem)) = RangeId
  InE (Edge ('Item RefRefItem)) = ReferenceResultId
  InE (Edge ('Item ImpRangeItem)) = RangeId
  InE (Edge ('Item ImpImpItem)) = ImplementationResultId
  InE (Edge 'RefersTo) = ResultSetId
  InE (Edge 'EMoniker) = MonikerId
  InE (Edge 'EPackageInformation) = PackageInformationId
  InE (Edge 'TextDocument_DocumentSymbol) = DocumentSymbolResultId
  InE (Edge 'TextDocument_FoldingRange) = FoldingRangeResultId
  InE (Edge 'TextDocument_DocumentLink) = DocumentLinkResultId
  InE (Edge ('TextDocument_Diagnostic a)) = DiagnosticResultId
  InE (Edge ('TextDocument_Definition a)) = DefinitionResultId
  InE (Edge ('TextDocument_Declaration a)) = DeclarationResultId
  InE (Edge ('TextDocument_TypeDefinition a)) = TypeDefinitionResultId
  InE (Edge ('TextDocument_Hover a)) = HoverResultId
  InE (Edge ('TextDocument_References a)) = ReferenceResultId
  InE (Edge ('TextDocument_Implementation a)) = ImplementationResultId
  InE a = Empty

-- | Type of the LSIF `Property` given the `ElementType`
type family Property (t :: ElementType) where
  Property (Edge (Item a)) = Maybe ItemEdgeProperties
  Property a = Empty

-- | Type of the LSIF `Label` given the `ElementType`
type family Label (t :: ElementType) :: Type where
  Label (Vertex l) = SVertexLabel l
  Label (Edge l) = SEdgeLabel l

-- | Type of the LSIF `Tag` given the `ElementType`
type family Tag (t :: ElementType) :: Type where
  Tag (Vertex ('Range Untagged)) = Empty
  Tag (Vertex ('Range rt)) = RangeTag rt
  Tag a                    = Empty

-- | Like WhenV, but checks if the ElementType is any kind of
-- `Range`
type family WhenRange (t :: ElementType) (a :: Type) :: Type where
  WhenRange (Vertex ('Range rt)) a = a
  WhenRange t          a = Empty

data SomeElement where
  SomeElement :: forall t. Element t -> SomeElement

{- UPDATING ELEMENT

Suppose a new kind of vertex is added to the spec

export interface LampShade extends V {

	label: VertexLabels.lampshade;

	kind: LampShadeKind;

	shade: Int;

}

1. Add `LampShade` to VertexLabel and SVertexLabel
2. Add a type for LampShadeKind along with a ToJSON instance
3. For any fields that already exist in Element(like `kind`), extend
the MatchV type family application to return the correct result.

Here, `_kind` goes from

  , _kind     :: MatchV t '[ '( 'Project,Text), '( 'Moniker,MonikerKind)]

to

  , _kind     :: MatchV t '[ '( 'Project,Text), '( 'Moniker,MonikerKind ), '( 'LampShade, LampShadeKind)]

4. for any new fields, add the field with the appropriate type guard:

  , _ shade :: WhenV t 'LampShade Int

WhenV and WhenVs are simply special cases for MatchV defined for
convienience. They can always be inlined to MatchV

If the MatchV is getting too large or unwieldy, you can also write a new type
family that returns the correct "kind" for "t" if "t" is supposed to have a
"kind" field, and `Empty` otherwise.

For example,

type family Kind t where
  Kind (Vertex 'Project) = Text
  Kind (Vertex 'Moniker) = MonikerKind
  Kind (Vertex 'LampShade) = LampShadeKind
  Kind a = Empty

5. Add a new type synonym

  type LampShade = V 'LampShade

6. Add the ToJSON instance

  instance A.ToJSON LampShade where
    toEncoding = A.genericToEncoding aesonOpts
    toJSON = A.genericToJSON aesonOpts

7. Add a constructor for LampShade

  mkLampshade :: LsifId -> LampShadeKind -> Int -> LampShade
  mkLampshade lid = mkConstr (Proxy @LampShade) lid SVertex SLampShade

-}

-- | The main type: An element in the graph
data Element (t :: ElementType)
  = Element
  { _id    :: LsifId (ElementId t)
  , _type  :: SElementType t

  , _label :: Label t

  , _tag   :: Tag t

  , _outV :: OutE t
  , _inV  :: InE t

  , _property :: Property t

  , _start :: WhenRange t LSP.Position
  , _end   :: WhenRange t LSP.Position

  , _range :: WhenV t 'Location LSP.Range

  , _version :: MatchV t '[ '( 'MetaData, Text ), '( 'PackageInformation, Maybe Text) ]
  , _projectRoot :: WhenV t 'MetaData (Maybe LSP.Uri)
  , _toolInfo :: WhenV t 'MetaData (Maybe ToolInfo)

  , _kind     :: MatchV t '[ '( 'Project,Text), '( 'Moniker,MonikerKind)]
  , _resource :: WhenV t 'Project (Maybe LSP.Uri)

  , _uri        :: MatchV t '[ '( 'Document, LSP.Uri), '( 'PackageInformation, Maybe LSP.Uri)]
  , _languageId :: WhenV t 'Document Text

  , _data     :: WhenVs t ['Project,'Document] (Maybe AdditionalData)
  , _contents :: WhenVs t ['Project,'Document,'PackageInformation] (Maybe Text)

  , _scheme :: WhenV t 'Moniker Text
  , _identifier :: WhenV t 'Moniker Text

  , _name :: WhenV t 'PackageInformation Text
  , _manager :: WhenV t 'PackageInformation Text
  , _repository :: WhenV t 'PackageInformation (Maybe Repository)

  , _declarations :: WhenV t 'ReferenceResult (Maybe [RangeId :|: LSP.Location])
  , _definitions  :: WhenV t 'ReferenceResult (Maybe [RangeId :|: LSP.Location])
  , _references   :: WhenV t 'ReferenceResult (Maybe [RangeId :|: LSP.Location])
  , _referenceResults :: WhenV t 'ReferenceResult (Maybe [LsifId (VertexId 'ReferenceResult)])

  , _implementationResults :: WhenV t 'ImplementationResult (Maybe [LsifId (VertexId 'ImplementationResult)])

  , _result :: Result t
  } deriving Generic

type family Result (t :: ElementType) :: Type where
  Result (Vertex 'DocumentSymbolResult) = [LSP.DocumentSymbol] :|: [RangeBasedDocumentSymbol]
  Result (Vertex 'DiagnosticResult    ) = [LSP.Diagnostic]
  Result (Vertex 'FoldingRangeResult  ) = [LSP.FoldingRange]
  Result (Vertex 'DocumentLinkResult  ) = [LSP.DocumentLink]
  Result (Vertex 'DeclarationResult   ) = Maybe [RangeId :|: LSP.Location]
  Result (Vertex 'DefinitionResult    ) = Maybe [RangeId :|: LSP.Location]
  Result (Vertex 'TypeDefinitionResult) = Maybe [RangeId :|: LSP.Location]
  Result (Vertex 'HoverResult         ) = LSP.Hover
  Result (Vertex 'ImplementationResult) = Maybe [RangeId :|: LSP.Location]
  Result t                              = Empty

type V a = Element (Vertex a)

type Range a = V ('Range a)

type DefinitionRange = Range 'Definition
type DeclarationRange = Range 'Declaration
type ReferenceRange = Range 'Reference
type UnknownRange = Range 'Unknown
type UntaggedRange = Range 'Untagged

type ResultSet            = V 'ResultSet
type Location             = V 'Location
type MetaData             = V 'MetaData
type Project              = V 'Project
type Document             = V 'Document
type Moniker              = V 'Moniker
type PackageInformation   = V 'PackageInformation
type DocumentSymbolResult = V 'DocumentSymbolResult
type FoldingRangeResult   = V 'FoldingRangeResult
type DocumentLinkResult   = V 'DocumentLinkResult
type DiagnosticResult     = V 'DiagnosticResult
type DeclarationResult    = V 'DeclarationResult
type DefinitionResult     = V 'DefinitionResult
type TypeDefinitionResult = V 'TypeDefinitionResult
type HoverResult          = V 'HoverResult
type ReferenceResult      = V 'ReferenceResult
type ImplementationResult = V 'ImplementationResult

mkDefinitionRange      lid = mkConstr (Proxy @DefinitionRange) lid SVertex SRange
mkDeclarationRange     lid = mkConstr (Proxy @DeclarationRange) lid SVertex SRange
mkReferenceRange       lid = mkConstr (Proxy @ReferenceRange) lid SVertex SRange
mkUnknownRange         lid = mkConstr (Proxy @UnknownRange) lid SVertex SRange
mkUntaggedRange        lid = mkConstr (Proxy @UntaggedRange) lid SVertex SRange

mkResultSet            lid = mkConstr (Proxy @ResultSet) lid SVertex SResultSet
mkLocation             lid = mkConstr (Proxy @Location) lid SVertex SLocation
mkMetaData             lid = mkConstr (Proxy @MetaData) lid SVertex SMetaData
mkProject              lid = mkConstr (Proxy @Project) lid SVertex SProject
mkDocument             lid = mkConstr (Proxy @Document) lid SVertex SDocument
mkMoniker              lid = mkConstr (Proxy @Moniker) lid SVertex SMoniker
mkPackageInformation   lid = mkConstr (Proxy @PackageInformation) lid SVertex SPackageInformation
mkDocumentSymbolResult lid = mkConstr (Proxy @DocumentSymbolResult) lid SVertex SDocumentSymbolResult
mkFoldingRangeResult   lid = mkConstr (Proxy @FoldingRangeResult) lid SVertex SFoldingRangeResult
mkDocumentLinkResult   lid = mkConstr (Proxy @DocumentLinkResult) lid SVertex SDocumentLinkResult
mkDiagnosticResult     lid = mkConstr (Proxy @DiagnosticResult) lid SVertex SDiagnosticResult
mkDeclarationResult    lid = mkConstr (Proxy @DeclarationResult) lid SVertex SDeclarationResult
mkDefinitionResult     lid = mkConstr (Proxy @DefinitionResult) lid SVertex SDefinitionResult
mkTypeDefinitionResult lid = mkConstr (Proxy @TypeDefinitionResult) lid SVertex STypeDefinitionResult
mkHoverResult          lid = mkConstr (Proxy @HoverResult) lid SVertex SHoverResult
mkReferenceResult      lid = mkConstr (Proxy @ReferenceResult) lid SVertex SReferenceResult
mkImplementationResult lid = mkConstr (Proxy @ImplementationResult) lid SVertex SImplementationResult

instance A.ToJSON DefinitionRange where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DeclarationRange where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ReferenceRange where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON UnknownRange where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON UntaggedRange where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ResultSet where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON Location where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON MetaData where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON Project where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON Document where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON Moniker where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON PackageInformation where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DocumentSymbolResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON FoldingRangeResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DocumentLinkResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DiagnosticResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DeclarationResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DefinitionResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON TypeDefinitionResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON HoverResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ReferenceResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ImplementationResult where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts

data ElementType = Vertex VertexLabel | Edge EdgeLabel
data SElementType t where
  SVertex :: SElementType (Vertex vl)
  SEdge   :: SElementType (Edge el)

instance A.ToJSON (SElementType a) where
  toJSON SVertex = A.toJSON "vertex"
  toJSON SEdge = A.toJSON "edge"

type RangeId = LsifId 'RangeId

-- | All known vertices label types
data VertexLabel
  = MetaData
  | Project
  | Range RangeTagType
  | Location
  | Document
  | Moniker
  | PackageInformation
  | ResultSet
  | DocumentSymbolResult
  | FoldingRangeResult
  | DocumentLinkResult
  | DiagnosticResult
  | DeclarationResult
  | DefinitionResult
  | TypeDefinitionResult
  | HoverResult
  | ReferenceResult
  | ImplementationResult

type MetaDataId = LsifId (VertexId 'MetaData)
type ProjectId = LsifId (VertexId 'Project)
type LocationId = LsifId (VertexId 'Location)
type DocumentId = LsifId (VertexId 'Document)
type MonikerId = LsifId (VertexId 'Moniker)
type PackageInformationId = LsifId (VertexId 'PackageInformation)
type ResultSetId = LsifId (VertexId 'ResultSet)
type DocumentSymbolResultId = LsifId (VertexId 'DocumentSymbolResult)
type FoldingRangeResultId = LsifId (VertexId 'FoldingRangeResult)
type DocumentLinkResultId = LsifId (VertexId 'DocumentLinkResult)
type DiagnosticResultId = LsifId (VertexId 'DiagnosticResult)
type DeclarationResultId = LsifId (VertexId 'DeclarationResult)
type DefinitionResultId = LsifId (VertexId 'DefinitionResult)
type TypeDefinitionResultId = LsifId (VertexId 'TypeDefinitionResult)
type HoverResultId = LsifId (VertexId 'HoverResult)
type ReferenceResultId = LsifId (VertexId 'ReferenceResult)
type ImplementationResultId = LsifId (VertexId 'ImplementationResult)


data SVertexLabel (v :: VertexLabel) where
  SMetaData             :: SVertexLabel 'MetaData
  SProject              :: SVertexLabel 'Project
  SRange                :: forall a. SVertexLabel ('Range a)
  SLocation             :: SVertexLabel 'Location
  SDocument             :: SVertexLabel 'Document
  SMoniker              :: SVertexLabel 'Moniker
  SPackageInformation   :: SVertexLabel 'PackageInformation
  SResultSet            :: SVertexLabel 'ResultSet
  SDocumentSymbolResult :: SVertexLabel 'DocumentSymbolResult
  SFoldingRangeResult   :: SVertexLabel 'FoldingRangeResult
  SDocumentLinkResult   :: SVertexLabel 'DocumentLinkResult
  SDiagnosticResult     :: SVertexLabel 'DiagnosticResult
  SDeclarationResult    :: SVertexLabel 'DeclarationResult
  SDefinitionResult     :: SVertexLabel 'DefinitionResult
  STypeDefinitionResult :: SVertexLabel 'TypeDefinitionResult
  SHoverResult          :: SVertexLabel 'HoverResult
  SReferenceResult      :: SVertexLabel 'ReferenceResult
  SImplementationResult :: SVertexLabel 'ImplementationResult

instance A.ToJSON (SVertexLabel a) where
  toJSON SMetaData             = A.toJSON "metaData"
  toJSON SProject              = A.toJSON "project"
  toJSON SRange                = A.toJSON "range"
  toJSON SLocation             = A.toJSON "location"
  toJSON SDocument             = A.toJSON "document"
  toJSON SMoniker              = A.toJSON "moniker"
  toJSON SPackageInformation   = A.toJSON "packageInformation"
  toJSON SResultSet            = A.toJSON "resultSet"
  toJSON SDocumentSymbolResult = A.toJSON "documentSymbolResult"
  toJSON SFoldingRangeResult   = A.toJSON "foldingRangeResult"
  toJSON SDocumentLinkResult   = A.toJSON "documentLinkResult"
  toJSON SDiagnosticResult     = A.toJSON "diagnosticResult"
  toJSON SDeclarationResult    = A.toJSON "declarationResult"
  toJSON SDefinitionResult     = A.toJSON "definitionResult"
  toJSON STypeDefinitionResult = A.toJSON "typeDefinitionResult"
  toJSON SHoverResult          = A.toJSON "hoverResult"
  toJSON SReferenceResult      = A.toJSON "referenceResult"
  toJSON SImplementationResult = A.toJSON "implementationResult"

data MonikerKind = Import | Export deriving Generic

instance A.ToJSON MonikerKind where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts

data RangeTagType
  = Declaration
  | Definition
  | Reference
  | Unknown
  | Untagged
  deriving Generic

instance A.ToJSON RangeTagType where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts

data SRangeTagType (rt :: RangeTagType) where
  SDeclaration :: SRangeTagType Declaration
  SDefinition  :: SRangeTagType Definition
  SReference   :: SRangeTagType Reference
  SUnknown     :: SRangeTagType Unknown

instance A.ToJSON (SRangeTagType rt) where
  toJSON SDeclaration = A.toJSON Declaration
  toJSON SDefinition  = A.toJSON Definition
  toJSON SReference   = A.toJSON Reference
  toJSON SUnknown     = A.toJSON Unknown

data ContainsType
  = ProjDocContains
  | DocRangeContains
  deriving (Eq,Ord,Generic,Show)

data ItemsType
  = RefRangeItem
  | RefRefItem
  | ImpRangeItem
  | ImpImpItem
  deriving (Eq,Ord,Generic,Show)

data DocOrProjDiag
  = DocDiag
  | ProjDiag
  deriving (Eq,Ord,Generic,Show)

data RangeOrResult
  = RangeEdge
  | ResultEdge
  deriving (Eq,Ord,Generic,Show)

data EdgeLabel
  = Contains ContainsType
  | Item ItemsType
  | RefersTo
  | EMoniker
  | EPackageInformation
  | TextDocument_DocumentSymbol
  | TextDocument_FoldingRange
  | TextDocument_DocumentLink
  | TextDocument_Diagnostic DocOrProjDiag
  | TextDocument_Definition RangeOrResult
  | TextDocument_Declaration RangeOrResult
  | TextDocument_TypeDefinition RangeOrResult
  | TextDocument_Hover RangeOrResult
  | TextDocument_References RangeOrResult
  | TextDocument_Implementation RangeOrResult
  deriving (Eq,Ord,Generic,Show)

type E l = Element (Edge l)

type ContainsEdge a = E ('Contains a)
type ContainsProjDocEdge = ContainsEdge 'ProjDocContains
type ContainsDocRangeEdge = ContainsEdge 'DocRangeContains

type ItemEdge a = E ('Item a)
type ItemRefRangeEdge = ItemEdge 'RefRangeItem
type ItemRefRefEdge = ItemEdge 'RefRefItem
type ItemImpRangeEdge = ItemEdge 'ImpRangeItem
type ItemImpImpEdge = ItemEdge 'ImpImpItem

type RefersToEdge = E 'RefersTo
type MonikerEdge = E 'EMoniker
type PackageInformationEdge = E 'EPackageInformation
type DocumentSymbolEdge = E 'TextDocument_DocumentSymbol
type FoldingRangeEdge = E 'TextDocument_FoldingRange
type DocumentLinkEdge = E 'TextDocument_DocumentLink

type DiagnosticEdge a = E ('TextDocument_Diagnostic a)
type DiagnosticDocEdge = DiagnosticEdge 'DocDiag
type DiagnosticProjEdge = DiagnosticEdge 'ProjDiag

type DefinitionRangeEdge = E ('TextDocument_Definition RangeEdge)
type DeclarationRangeEdge = E ('TextDocument_Declaration RangeEdge)
type TypeDefinitionRangeEdge = E ('TextDocument_TypeDefinition RangeEdge)
type HoverRangeEdge = E ('TextDocument_Hover RangeEdge)
type ReferencesRangeEdge = E ('TextDocument_References RangeEdge)
type ImplementationRangeEdge = E ('TextDocument_Implementation RangeEdge)

type DefinitionResultEdge = E ('TextDocument_Definition ResultEdge)
type DeclarationResultEdge = E ('TextDocument_Declaration ResultEdge)
type TypeDefinitionResultEdge = E ('TextDocument_TypeDefinition ResultEdge)
type HoverResultEdge = E ('TextDocument_Hover ResultEdge)
type ReferencesResultEdge = E ('TextDocument_References ResultEdge)
type ImplementationResultEdge = E ('TextDocument_Implementation ResultEdge)

instance A.ToJSON ContainsProjDocEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ContainsDocRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ItemRefRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ItemRefRefEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ItemImpRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ItemImpImpEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON RefersToEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON MonikerEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON PackageInformationEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DocumentSymbolEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON FoldingRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DocumentLinkEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DiagnosticDocEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DiagnosticProjEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DefinitionRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DeclarationRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON TypeDefinitionRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON HoverRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ReferencesRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ImplementationRangeEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DefinitionResultEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DeclarationResultEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON TypeDefinitionResultEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON HoverResultEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ReferencesResultEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ImplementationResultEdge where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts

mkContainsProjDocEdge lid
  = mkConstr (Proxy @ContainsProjDocEdge) lid SEdge SContains
mkContainsDocRangeEdge lid
  = mkConstr (Proxy @ContainsDocRangeEdge) lid SEdge SContains
mkItemRefRangeEdge lid =
  mkConstr (Proxy @ItemRefRangeEdge) lid SEdge SItem
mkItemRefRefEdge lid =
  mkConstr (Proxy @ItemRefRefEdge) lid SEdge SItem
mkItemImpRangeEdge lid =
  mkConstr (Proxy @ItemImpRangeEdge) lid SEdge SItem
mkItemImpImpEdge lid =
  mkConstr (Proxy @ItemImpImpEdge) lid SEdge SItem
mkRefersToEdge lid
  = mkConstr (Proxy @RefersToEdge) lid SEdge SRefersTo
mkMonikerEdge lid
  = mkConstr (Proxy @MonikerEdge) lid SEdge SEMoniker
mkPackageInformationEdge lid
  = mkConstr (Proxy @PackageInformationEdge) lid SEdge SEPackageInformation
mkDocumentSymbolEdge lid
  = mkConstr (Proxy @DocumentSymbolEdge) lid SEdge STextDocument_DocumentSymbol
mkFoldingRangeEdge lid
  = mkConstr (Proxy @FoldingRangeEdge) lid SEdge STextDocument_FoldingRange
mkDocumentLinkEdge lid
  = mkConstr (Proxy @DocumentLinkEdge) lid SEdge STextDocument_DocumentLink
mkDiagnosticDocEdge lid
  = mkConstr (Proxy @DiagnosticDocEdge) lid SEdge STextDocument_Diagnostic
mkDiagnosticProjEdge lid
  = mkConstr (Proxy @DiagnosticProjEdge) lid SEdge STextDocument_Diagnostic
mkDefinitionRangeEdge lid
  = mkConstr (Proxy @DefinitionRangeEdge) lid SEdge STextDocument_Definition
mkDeclarationRangeEdge lid
  = mkConstr (Proxy @DeclarationRangeEdge) lid SEdge STextDocument_Declaration
mkTypeDefinitionRangeEdge lid
  = mkConstr (Proxy @TypeDefinitionRangeEdge) lid SEdge STextDocument_TypeDefinition
mkHoverRangeEdge lid
  = mkConstr (Proxy @HoverRangeEdge) lid SEdge STextDocument_Hover
mkReferencesRangeEdge lid
  = mkConstr (Proxy @ReferencesRangeEdge) lid SEdge STextDocument_References
mkImplementationRangeEdge lid
  = mkConstr (Proxy @ImplementationRangeEdge) lid SEdge STextDocument_Implementation
mkDefinitionResultEdge lid
  = mkConstr (Proxy @DefinitionResultEdge) lid SEdge STextDocument_Definition
mkDeclarationResultEdge lid
  = mkConstr (Proxy @DeclarationResultEdge) lid SEdge STextDocument_Declaration
mkTypeDefinitionResultEdge lid
  = mkConstr (Proxy @TypeDefinitionResultEdge) lid SEdge STextDocument_TypeDefinition
mkHoverResultEdge lid
  = mkConstr (Proxy @HoverResultEdge) lid SEdge STextDocument_Hover
mkReferencesResultEdge lid
  = mkConstr (Proxy @ReferencesResultEdge) lid SEdge STextDocument_References
mkImplementationResultEdge lid
  = mkConstr (Proxy @ImplementationResultEdge) lid SEdge STextDocument_Implementation

data SEdgeLabel (el :: EdgeLabel) where
  SContains :: SEdgeLabel (Contains a)
  SItem :: SEdgeLabel (Item a)
  SRefersTo :: SEdgeLabel RefersTo
  SEMoniker :: SEdgeLabel EMoniker
  SEPackageInformation :: SEdgeLabel EPackageInformation
  STextDocument_DocumentSymbol :: SEdgeLabel TextDocument_DocumentSymbol
  STextDocument_FoldingRange :: SEdgeLabel TextDocument_FoldingRange
  STextDocument_DocumentLink :: SEdgeLabel TextDocument_DocumentLink
  STextDocument_Diagnostic :: SEdgeLabel (TextDocument_Diagnostic a)
  STextDocument_Definition :: SEdgeLabel (TextDocument_Definition a)
  STextDocument_Declaration :: SEdgeLabel (TextDocument_Declaration a)
  STextDocument_TypeDefinition :: SEdgeLabel (TextDocument_TypeDefinition a)
  STextDocument_Hover :: SEdgeLabel (TextDocument_Hover a)
  STextDocument_References :: SEdgeLabel (TextDocument_References a)
  STextDocument_Implementation :: SEdgeLabel (TextDocument_Implementation a)

instance A.ToJSON (SEdgeLabel (el :: EdgeLabel)) where
  toJSON SContains = A.toJSON "contains"
  toJSON SItem = A.toJSON "item"
  toJSON SRefersTo = A.toJSON "refersTo"
  toJSON SEMoniker = A.toJSON "moniker"
  toJSON SEPackageInformation = A.toJSON "packageInformation"
  toJSON STextDocument_DocumentSymbol = A.toJSON "textDocument/documentSymbol"
  toJSON STextDocument_FoldingRange = A.toJSON "textDocument/foldingRange"
  toJSON STextDocument_DocumentLink = A.toJSON "textDocument/documentLink"
  toJSON STextDocument_Diagnostic = A.toJSON "textDocument/diagnostic"
  toJSON STextDocument_Definition = A.toJSON "textDocument/definition"
  toJSON STextDocument_Declaration = A.toJSON "textDocument/declaration"
  toJSON STextDocument_TypeDefinition = A.toJSON "textDocument/typeDefinition"
  toJSON STextDocument_Hover = A.toJSON "textDocument/hover"
  toJSON STextDocument_References = A.toJSON "textDocument/references"
  toJSON STextDocument_Implementation = A.toJSON "textDocument/implementation"

type family When2 (a :: k) (b :: k) (c :: k) (t :: Type) :: Type where
  When2 a a b t = t
  When2 a b a t = t
  When2 a b c t = Empty

data RangeTag (t :: RangeTagType)
  = RangeTag
  { _type        :: SRangeTagType t
  , _text        :: Text
  , _kind        :: When2 t Declaration Definition LSP.SymbolKind
  , _deprecated  :: When2 t Declaration Definition (Maybe Bool)
  , _fullRange   :: When2 t Declaration Definition LSP.Range
  , _detail      :: When2 t Declaration Definition (Maybe Text)
  } deriving Generic

type DeclarationTag = RangeTag Declaration
type DefinitionTag  = RangeTag Definition
type ReferenceTag   = RangeTag Reference
type UnknownTag     = RangeTag Unknown

mkDeclarationTag = mkConstr (Proxy @DeclarationTag) SDeclaration
mkDefinitionTag  = mkConstr (Proxy @DefinitionTag) SDefinition
mkReferenceTag   = mkConstr (Proxy @ReferenceTag) SReference
mkUnknownTag     = mkConstr (Proxy @UnknownTag) SUnknown

instance A.ToJSON DeclarationTag where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON DefinitionTag where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON ReferenceTag where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
instance A.ToJSON UnknownTag where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts


data RangeBasedDocumentSymbol
  = RangeBasedDocumentSymbol
  { _id :: RangeId
  , _children :: Maybe [RangeBasedDocumentSymbol]
  } deriving (Generic)

instance A.ToJSON RangeBasedDocumentSymbol where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts

data ToolInfo
  = ToolInfo
  { _name :: Text
  , _args :: Maybe [Text]
  } deriving (Show, Generic)

instance A.ToJSON ToolInfo where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts

type AdditionalData = A.Value

data Repository
  = Repository
  { _type :: Text
  , _url :: LSP.Uri
  , _commitId :: Maybe Text
  } deriving (Eq,Ord,Show,Generic)

instance A.ToJSON Repository where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts

data ItemEdgeProperties
  = Declarations
  | Definitions
  | References
  | ReferenceResults
  | ImplementationResults
  deriving (Eq,Ord,Show,Generic,Enum)

instance A.ToJSON ItemEdgeProperties where
  toEncoding = A.genericToEncoding aesonOpts
  toJSON = A.genericToJSON aesonOpts
