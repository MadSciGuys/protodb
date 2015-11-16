{-|
Module      : ProtoDB.Writer
Description : ProtoDB Datablock Writer and Associated Types
Copyright   : Travis Whitaker 2015
License     : MIT
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX
-}

module ProtoDB.Writer (
    expandTypes
  , mkProtoDB
  , putProtoCell
  , toProtoField
  , WritableField(..)
  , WritableDB(..)
  , writeDB
  ) where

import Data.Either

import qualified Data.Sequence as S

import Text.ProtocolBuffers.Basic

import ProtoDB.Types

import Data.ProtoBlob

import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

data WritableField = WritableField {
    wfTitle  :: T.Text
  , wfType   :: ProtoCellType
  , wfVector :: [Int]
  } deriving (Eq, Ord, Show, Read)

data WritableDB = WritableDB {
    wdbTitle  :: T.Text
  , wdbFields :: [WritableField]
  , wdbCells  :: [ProtoCell]
  } deriving (Eq, Ord, Show)

-- | Convert a 'T.Text' to a 'Uft8'. This first unpacks the 'T.Text' into a lazy
--   'B.ByteString', which is then checked for UTF8 conformance. This is all
--   indefensibly inefficient.
textToUtf8 :: T.Text -> Utf8
textToUtf8 = either (error em) id . toUtf8 . B.fromStrict . TE.encodeUtf8
    where em = "textToUtf8: invalid UTF8"

toProtoDB :: WritableDB -> ProtoDB
toProtoDB (WritableDB t fs _) = mkProtoDB t fs

mkProtoDB :: T.Text -> [WritableField] -> ProtoDB
mkProtoDB t fs = ProtoDB (textToUtf8 t) (fromIntegral (length fs))

toProtoField :: WritableField -> ProtoField
toProtoField (WritableField t tp vs) = ProtoField (textToUtf8 t)
                                                  (protoCellTypeProtoType tp)
                                                  (S.fromList $ map fromIntegral vs)

putProtoCell :: ProtoCell -> PutBlob
putProtoCell (ProtoIntCell p)      = lenMessagePutM p
putProtoCell (ProtoRealCell p)     = lenMessagePutM p
putProtoCell (ProtoStringCell p)   = lenMessagePutM p
putProtoCell (ProtoDateTimeCell p) = lenMessagePutM p
putProtoCell (ProtoBinaryCell p)   = lenMessagePutM p

expandTypes :: [WritableField] -> [ProtoCellType]
expandTypes = concatMap $ \ field@(WritableField _ cellType vectorShape) 
  -> replicate (product vectorShape) cellType

writeDB :: WritableDB -> PutBlob
writeDB w@(WritableDB _ fields cells) = do
  lenMessagePutM $ toProtoDB w
  mapM_ (lenMessagePutM . toProtoField) fields
  mapM_ (uncurry putCell) $ zip (concat $ repeat $ expandTypes fields) cells
  where putCell pct c = if pct `protoTypeMatch` c
                          then putProtoCell c
                          else error "writeDB: type mismatch."
