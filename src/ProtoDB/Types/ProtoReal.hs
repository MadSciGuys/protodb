{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoReal (ProtoReal(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ProtoReal = ProtoReal{payloaddouble :: !(P'.Double)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoReal where
  mergeAppend (ProtoReal x'1) (ProtoReal y'1) = ProtoReal (P'.mergeAppend x'1 y'1)
 
instance P'.Default ProtoReal where
  defaultValue = ProtoReal P'.defaultValue
 
instance P'.Wire ProtoReal where
  wireSize ft' self'@(ProtoReal x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 1 x'1)
  wirePut ft' self'@(ProtoReal x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 9 1 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{payloaddouble = new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoReal) ProtoReal where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoReal
 
instance P'.ReflectDescriptor ProtoReal where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [9]) (P'.fromDistinctAscList [9])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protodb.ProtoReal\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoReal\"}, descFilePath = [\"Protodb\",\"ProtoReal.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoReal.payloaddouble\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoReal\"], baseName' = FName \"payloaddouble\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ProtoReal where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ProtoReal where
  textPut msg
   = do
       P'.tellT "payloaddouble" (payloaddouble msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'payloaddouble]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'payloaddouble
         = P'.try
            (do
               v <- P'.getT "payloaddouble"
               Prelude'.return (\ o -> o{payloaddouble = v}))
