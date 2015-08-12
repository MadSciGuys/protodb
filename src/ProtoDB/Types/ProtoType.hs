{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoType (ProtoType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ProtoType = Int
               | Real
               | String
               | DateTime
               | Binary
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoType
 
instance Prelude'.Bounded ProtoType where
  minBound = Int
  maxBound = Binary
 
instance P'.Default ProtoType where
  defaultValue = Int
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe ProtoType
toMaybe'Enum 1 = Prelude'.Just Int
toMaybe'Enum 2 = Prelude'.Just Real
toMaybe'Enum 3 = Prelude'.Just String
toMaybe'Enum 4 = Prelude'.Just DateTime
toMaybe'Enum 5 = Prelude'.Just Binary
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum ProtoType where
  fromEnum Int = 1
  fromEnum Real = 2
  fromEnum String = 3
  fromEnum DateTime = 4
  fromEnum Binary = 5
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Protodb.ProtoType") . toMaybe'Enum
  succ Int = Real
  succ Real = String
  succ String = DateTime
  succ DateTime = Binary
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Protodb.ProtoType"
  pred Real = Int
  pred String = Real
  pred DateTime = String
  pred Binary = DateTime
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Protodb.ProtoType"
 
instance P'.Wire ProtoType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB ProtoType
 
instance P'.MessageAPI msg' (msg' -> ProtoType) ProtoType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum ProtoType where
  reflectEnum = [(1, "Int", Int), (2, "Real", Real), (3, "String", String), (4, "DateTime", DateTime), (5, "Binary", Binary)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".protodb.ProtoType") [] ["Protodb"] "ProtoType") ["Protodb", "ProtoType.hs"]
      [(1, "Int"), (2, "Real"), (3, "String"), (4, "DateTime"), (5, "Binary")]
 
instance P'.TextType ProtoType where
  tellT = P'.tellShow
  getT = P'.getRead
