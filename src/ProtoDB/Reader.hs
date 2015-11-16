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
  , readRowIndex
  , lazyForceReadDB
  , forceReadDB
  , forceReadDBIndex
  ) where

--import Data.Either

import Control.Monad

import Data.Foldable

import qualified Data.Sequence as S

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Get
import Text.ProtocolBuffers.WireMessage (runGetOnLazy)

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

-- | Return not only the row, but the row start byte offset and size in bytes.
readRowIndex :: [ProtoCellType] -> GetBlob ([ProtoCell], Int, Int)
readRowIndex ts = do
    s <- fromIntegral <$> bytesRead
    r <- readRow ts
    f <- fromIntegral <$> bytesRead
    return (r, s, f)

lazyForceReadDB :: B.ByteString -> (ReadDB, [[ProtoCell]])
lazyForceReadDB = run' readDB $ \ rdb@(ReadDB _ flds _) cells -> 
  let go remainder0 = if B.null remainder0 then [] else run' 
        (readRow $ map rfType $ flds)
        (\ row -> (row:) . go )
        remainder0
  in (rdb, go cells)
  where run' getBlob next = either error (uncurry next) . runGetOnLazy getBlob

forceReadDB :: GetBlob (ReadDB, [[ProtoCell]])
forceReadDB = readDB >>= (\rdb -> readRows (map rfType (rdbFields rdb)) >>= (\rs -> return (rdb, rs)))
    where readRows ts = isReallyEmpty >>= \case True  -> return []
                                                False -> liftM2 (:) (readRow ts) (readRows ts)

forceReadDBIndex :: GetBlob (ReadDB, [([ProtoCell], Int, Int)])
forceReadDBIndex = readDB >>= (\rdb -> readRows (map rfType (rdbFields rdb)) >>= (\rs -> return (rdb, rs)))
    where readRows ts = isReallyEmpty >>= \case True  -> return []
                                                False -> liftM2 (:) (readRowIndex ts) (readRows ts)
