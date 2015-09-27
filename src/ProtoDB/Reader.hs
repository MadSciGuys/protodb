{-|
Module      : ProtoDB.Reader
Description : ProtoDB Datablock Reader and Associated Types
Copyright   : Travis Whitaker 2015
License     : MIT
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX
-}

{-# LANGUAGE LambdaCase #-}

module ProtoDB.Reader (
    ReadField(..)
  , ReadDB(..)
  , readDB
  , readRow
  , forceReadDB
  ) where

--import Data.Either

import Control.Monad

import Data.Foldable

import qualified Data.Sequence as S

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Get

import ProtoDB.Types

import Data.ProtoBlob

import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

-- | Metadata for a field we have read, i.e. "read" is in the past tense.
data ReadField = ReadField {
    rfTitle  :: T.Text
  , rfType   :: ProtoCellType
  , rfVector :: [Int]
  } deriving (Eq, Ord, Show)

-- | Metadata for a datablock we have read, i.e. "read" is in the past tense.
data ReadDB = ReadDB {
    rdbTitle  :: T.Text
  , rdbFields :: [ReadField]
  , rdbStart  :: Int
  } deriving (Eq, Ord, Show)

-- | Convert a 'Utf8' to a 'T.Text'.
utf8ToText :: Utf8 -> T.Text
utf8ToText = TE.decodeUtf8 . B.toStrict . utf8

toReadField :: ProtoField -> ReadField
toReadField (ProtoField t pt vs) = ReadField (utf8ToText t)
                                             (protoTypeProtoCellType pt)
                                             (map fromIntegral (toList vs))

readDB :: GetBlob ReadDB
readDB = do
    (ProtoDB t fn) <- lenMessageGetM
    dfs            <- replicateM (fromIntegral fn) lenMessageGetM
    i              <- bytesRead
    return $ ReadDB (utf8ToText t) (map toReadField dfs) (fromIntegral i)

readRow :: [ProtoCellType] -> GetBlob [ProtoCell]
readRow = mapM readCell
    where readCell ProtoIntType      = fmap ProtoIntCell lenMessageGetM
          readCell ProtoRealType     = fmap ProtoRealCell lenMessageGetM
          readCell ProtoStringType   = fmap ProtoStringCell lenMessageGetM
          readCell ProtoDateTimeType = fmap ProtoDateTimeCell lenMessageGetM
          readCell ProtoBinaryType   = fmap ProtoDateTimeCell lenMessageGetM

forceReadDB :: GetBlob (ReadDB, [[ProtoCell]])
forceReadDB = readDB >>= (\rdb -> readRows (map rfType (rdbFields rdb)) >>= (\rs -> return (rdb, rs)))
    where readRows ts = isReallyEmpty >>= \case True  -> return []
                                                False -> liftM2 (:) (readRow ts) (readRows ts)
