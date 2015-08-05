{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoInt (ProtoInt(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ProtoInt = ProtoInt{payloadint :: !(P'.Maybe P'.Int64)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoInt where
  mergeAppend (ProtoInt x'1) (ProtoInt y'1) = ProtoInt (P'.mergeAppend x'1 y'1)
 
instance P'.Default ProtoInt where
  defaultValue = ProtoInt P'.defaultValue
 
instance P'.Wire ProtoInt where
  wireSize ft' self'@(ProtoInt x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 18 x'1)
  wirePut ft' self'@(ProtoInt x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 18 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{payloadint = Prelude'.Just new'Field}) (P'.wireGet 18)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoInt) ProtoInt where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoInt
 
instance P'.ReflectDescriptor ProtoInt where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protodb.ProtoInt\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoInt\"}, descFilePath = [\"Protodb\",\"ProtoInt.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoInt.payloadint\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoInt\"], baseName' = FName \"payloadint\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ProtoInt where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ProtoInt where
  textPut msg
   = do
       P'.tellT "payloadint" (payloadint msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'payloadint]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'payloadint
         = P'.try
            (do
               v <- P'.getT "payloadint"
               Prelude'.return (\ o -> o{payloadint = v}))
