{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoDateTime (ProtoDateTime(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ProtoDateTime = ProtoDateTime{payloaddatetime :: !(P'.Maybe P'.Word64)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoDateTime where
  mergeAppend (ProtoDateTime x'1) (ProtoDateTime y'1) = ProtoDateTime (P'.mergeAppend x'1 y'1)
 
instance P'.Default ProtoDateTime where
  defaultValue = ProtoDateTime P'.defaultValue
 
instance P'.Wire ProtoDateTime where
  wireSize ft' self'@(ProtoDateTime x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 4 x'1)
  wirePut ft' self'@(ProtoDateTime x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 4 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{payloaddatetime = Prelude'.Just new'Field}) (P'.wireGet 4)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoDateTime) ProtoDateTime where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoDateTime
 
instance P'.ReflectDescriptor ProtoDateTime where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protodb.ProtoDateTime\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoDateTime\"}, descFilePath = [\"Protodb\",\"ProtoDateTime.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoDateTime.payloaddatetime\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoDateTime\"], baseName' = FName \"payloaddatetime\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ProtoDateTime where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ProtoDateTime where
  textPut msg
   = do
       P'.tellT "payloaddatetime" (payloaddatetime msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'payloaddatetime]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'payloaddatetime
         = P'.try
            (do
               v <- P'.getT "payloaddatetime"
               Prelude'.return (\ o -> o{payloaddatetime = v}))
