{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoBinary (ProtoBinary(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ProtoBinary = ProtoBinary{parloadbytes :: !(P'.Maybe P'.ByteString)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoBinary where
  mergeAppend (ProtoBinary x'1) (ProtoBinary y'1) = ProtoBinary (P'.mergeAppend x'1 y'1)
 
instance P'.Default ProtoBinary where
  defaultValue = ProtoBinary P'.defaultValue
 
instance P'.Wire ProtoBinary where
  wireSize ft' self'@(ProtoBinary x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 12 x'1)
  wirePut ft' self'@(ProtoBinary x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 12 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{parloadbytes = Prelude'.Just new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoBinary) ProtoBinary where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoBinary
 
instance P'.ReflectDescriptor ProtoBinary where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protodb.ProtoBinary\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoBinary\"}, descFilePath = [\"Protodb\",\"ProtoBinary.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoBinary.parloadbytes\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoBinary\"], baseName' = FName \"parloadbytes\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ProtoBinary where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ProtoBinary where
  textPut msg
   = do
       P'.tellT "parloadbytes" (parloadbytes msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'parloadbytes]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'parloadbytes
         = P'.try
            (do
               v <- P'.getT "parloadbytes"
               Prelude'.return (\ o -> o{parloadbytes = v}))
