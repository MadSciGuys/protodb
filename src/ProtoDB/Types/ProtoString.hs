{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoString (ProtoString(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ProtoString = ProtoString{payloadstring :: !(P'.Maybe P'.Utf8)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoString where
  mergeAppend (ProtoString x'1) (ProtoString y'1) = ProtoString (P'.mergeAppend x'1 y'1)
 
instance P'.Default ProtoString where
  defaultValue = ProtoString P'.defaultValue
 
instance P'.Wire ProtoString where
  wireSize ft' self'@(ProtoString x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1)
  wirePut ft' self'@(ProtoString x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 9 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{payloadstring = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoString) ProtoString where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoString
 
instance P'.ReflectDescriptor ProtoString where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protodb.ProtoString\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoString\"}, descFilePath = [\"Protodb\",\"ProtoString.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoString.payloadstring\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoString\"], baseName' = FName \"payloadstring\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ProtoString where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ProtoString where
  textPut msg
   = do
       P'.tellT "payloadstring" (payloadstring msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'payloadstring]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'payloadstring
         = P'.try
            (do
               v <- P'.getT "payloadstring"
               Prelude'.return (\ o -> o{payloadstring = v}))
