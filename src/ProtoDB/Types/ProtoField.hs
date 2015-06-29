{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ProtoDB.Types.ProtoField (ProtoField(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified ProtoDB.Types.ProtoType as Protodb (ProtoType)
 
data ProtoField = ProtoField{fieldtitle :: !(P'.Utf8), fieldtype :: !(Protodb.ProtoType), vectorshape :: !(P'.Seq P'.Word32)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoField where
  mergeAppend (ProtoField x'1 x'2 x'3) (ProtoField y'1 y'2 y'3)
   = ProtoField (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default ProtoField where
  defaultValue = ProtoField P'.defaultValue (Prelude'.read "Binary") P'.defaultValue
 
instance P'.Wire ProtoField where
  wireSize ft' self'@(ProtoField x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 14 x'2 + P'.wireSizePacked 1 13 x'3)
  wirePut ft' self'@(ProtoField x'1 x'2 x'3)
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
             P'.wirePutReq 16 14 x'2
             P'.wirePutPacked 26 13 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{fieldtitle = new'Field}) (P'.wireGet 9)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{fieldtype = new'Field}) (P'.wireGet 14)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{vectorshape = P'.append (vectorshape old'Self) new'Field})
                    (P'.wireGet 13)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{vectorshape = P'.mergeAppend (vectorshape old'Self) new'Field})
                    (P'.wireGetPacked 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoField) ProtoField where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoField
 
instance P'.ReflectDescriptor ProtoField where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 16]) (P'.fromDistinctAscList [10, 16, 24, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".protodb.ProtoField\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoField\"}, descFilePath = [\"Protodb\",\"ProtoField.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoField.fieldtitle\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoField\"], baseName' = FName \"fieldtitle\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoField.fieldtype\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoField\"], baseName' = FName \"fieldtype\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".protodb.ProtoType\", haskellPrefix = [], parentModule = [MName \"Protodb\"], baseName = MName \"ProtoType\"}), hsRawDefault = Just \"Binary\", hsDefault = Just (HsDef'Enum \"Binary\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".protodb.ProtoField.vectorshape\", haskellPrefix' = [], parentModule' = [MName \"Protodb\",MName \"ProtoField\"], baseName' = FName \"vectorshape\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Just (WireTag {getWireTag = 24},WireTag {getWireTag = 26}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ProtoField where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ProtoField where
  textPut msg
   = do
       P'.tellT "fieldtitle" (fieldtitle msg)
       P'.tellT "fieldtype" (fieldtype msg)
       P'.tellT "vectorshape" (vectorshape msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'fieldtitle, parse'fieldtype, parse'vectorshape]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'fieldtitle
         = P'.try
            (do
               v <- P'.getT "fieldtitle"
               Prelude'.return (\ o -> o{fieldtitle = v}))
        parse'fieldtype
         = P'.try
            (do
               v <- P'.getT "fieldtype"
               Prelude'.return (\ o -> o{fieldtype = v}))
        parse'vectorshape
         = P'.try
            (do
               v <- P'.getT "vectorshape"
               Prelude'.return (\ o -> o{vectorshape = P'.append (vectorshape o) v}))
