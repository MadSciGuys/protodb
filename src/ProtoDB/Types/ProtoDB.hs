{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoDB (ProtoDB(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ProtoDB = ProtoDB{dbtitle :: !(P'.Utf8), fields :: !(P'.Word32)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoDB where
  mergeAppend (ProtoDB x'1 x'2) (ProtoDB y'1 y'2) = ProtoDB (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default ProtoDB where
  defaultValue = ProtoDB P'.defaultValue P'.defaultValue
 
instance P'.Wire ProtoDB where
  wireSize ft' self'@(ProtoDB x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 13 x'2)
  wirePut ft' self'@(ProtoDB x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 9 x'1
             P'.wirePutReq 16 13 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{dbtitle = new'Field}) (P'.wireGet 9)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{fields = new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoDB) ProtoDB where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoDB
 
instance P'.ReflectDescriptor ProtoDB where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 16]) (P'.fromDistinctAscList [10, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protodb.ProtoDB\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoDB\"}, descFilePath = [\"Protodb\",\"ProtoDB.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoDB.dbtitle\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoDB\"], baseName' = FName \"dbtitle\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoDB.fields\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoDB\"], baseName' = FName \"fields\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ProtoDB where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ProtoDB where
  textPut msg
   = do
       P'.tellT "dbtitle" (dbtitle msg)
       P'.tellT "fields" (fields msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'dbtitle, parse'fields]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'dbtitle
         = P'.try
            (do
               v <- P'.getT "dbtitle"
               Prelude'.return (\ o -> o{dbtitle = v}))
        parse'fields
         = P'.try
            (do
               v <- P'.getT "fields"
               Prelude'.return (\ o -> o{fields = v}))
