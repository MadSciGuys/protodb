{-|
Module      : ProtoDB.Reader
Description : ProtoDB Datablock Reader and Associated Types
Copyright   : Travis Whitaker 2015
License     : MIT
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX
-}

{-# LANGUAGE LambdaCase, TupleSections #-}

module ProtoDB.Reader (
    ReadField(..)
  , ReadDB(..)
  , readDB
  , readRow
  , readRowIndex
  , lazyForceReadDB
  , forceReadDB
  , lazyForceReadDB
  , forceReadDBIndex
  , lazyForceReadDBIndex
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

import qualified Data.ByteString.Char8      as BS
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
    return (r, s, (f - s))

--lazyForceReadDB :: B.ByteString -> (ReadDB, [[ProtoCell]])
--lazyForceReadDB = run' readDB $ \ rdb@(ReadDB _ flds _) cells -> 
--  let go remainder0 = if B.null remainder0 then [] else run' 
--        (readRow $ map rfType $ flds)
--        (\ row -> (row:) . go )
--        remainder0
--  in (rdb, go cells)
--  where run' getBlob next = either error (uncurry next) . runGetOnLazy getBlob

forceReadDB :: GetBlob (ReadDB, [[ProtoCell]])
forceReadDB = readDB >>= (\rdb -> readRows (map rfType (rdbFields rdb)) >>= (\rs -> return (rdb, rs)))
    where readRows ts = isReallyEmpty >>= \case True  -> return []
                                                False -> liftM2 (:) (readRow ts) (readRows ts)

lazyForceReadDB :: B.ByteString -> Either String (ReadDB, [(Either String [ProtoCell])])
lazyForceReadDB = (readRows =<<) . readDB'
    where readDB' :: B.ByteString -> Either String ([B.ByteString], ReadDB)
          readDB' = go1 (runGet readDB B.empty) . map B.fromStrict . B.toChunks
          go1 :: Result ReadDB -> [B.ByteString] -> Either String ([B.ByteString], ReadDB)
          go1 (Failed _ e)       _      = Left e
          go1 (Partial cnt)      (c:cs) = go1 (cnt (Just c)) cs
          go1 (Partial cnt)      []     = go1 (cnt Nothing) []
          go1 (Finished c _ rdb) cs     = Right (c:cs, rdb)
          readRows :: ([B.ByteString], ReadDB) -> Either String (ReadDB, [(Either String [ProtoCell])])
          readRows (cs, rdb) = Right (rdb, go2 (dup (runGet (readRow (map rfType (rdbFields rdb))) B.empty)) cs)
          go2 :: (Result [ProtoCell], Result [ProtoCell]) -> [B.ByteString] -> [(Either String [ProtoCell])]
          go2 (_, (Failed _ e))     _      = [Left e]
          go2 (i, (Partial cnt))    (c:cs) = go2 (i, cnt (Just c)) cs
          go2 (i, (Partial cnt))    []     = go2 (i, cnt Nothing) []
          go2 (i, (Finished c _ r)) []
                    | B.null c             = [Right r]
                    | otherwise            = Right r : go2 (i, i) [c]
          go2 (i, (Finished c _ r)) cs     = Right r : go2 (i, i) (c:cs)
          dup :: a -> (a, a)
          dup x = (x, x)

forceReadDBIndex :: GetBlob (ReadDB, [([ProtoCell], Int, Int)])
forceReadDBIndex = readDB >>= (\rdb -> readRows (map rfType (rdbFields rdb)) >>= (\rs -> return (rdb, rs)))
    where readRows ts = isReallyEmpty >>= \case True  -> return []
                                                False -> liftM2 (:) (readRowIndex ts) (readRows ts)

lazyForceReadDBIndex :: B.ByteString -> Either String (ReadDB, [(Either String ([ProtoCell], Int, Int))])
lazyForceReadDBIndex = (readRows =<<) . readDB'
    where readDB' :: B.ByteString -> Either String ([B.ByteString], ReadDB)
          readDB' = go1 (runGet readDB B.empty) . map B.fromStrict . B.toChunks
          go1 :: Result ReadDB -> [B.ByteString] -> Either String ([B.ByteString], ReadDB)
          go1 (Failed _ e)       _      = Left e
          go1 (Partial cnt)      (c:cs) = go1 (cnt (Just c)) cs
          go1 (Partial cnt)      []     = go1 (cnt Nothing) []
          go1 (Finished c _ rdb) cs     = Right (c:cs, rdb)
          readRows :: ([B.ByteString], ReadDB) -> Either String (ReadDB, [(Either String ([ProtoCell], Int, Int))])
          readRows (cs, rdb) = Right (rdb, go2 (rdbStart rdb) (dup (runGet (readRow (map rfType (rdbFields rdb))) B.empty)) cs)
          go2 :: Int -> (Result [ProtoCell], Result [ProtoCell]) -> [B.ByteString] -> [(Either String ([ProtoCell], Int, Int))]
          go2 _ (_, (Failed _ e))     _      = [Left e]
          go2 b (i, (Partial cnt))    (c:cs) = go2 b (i, cnt (Just c)) cs
          go2 b (i, (Partial cnt))    []     = go2 b (i, cnt Nothing) []
          go2 b (i, (Finished c b' r)) []
                    | B.null c               = let b'' = fromIntegral b'
                                               in [Right (r, b, b+b'')]
                    | otherwise              = let b'' = fromIntegral b'
                                               in Right (r, b, b+b'') : go2 b'' (i, i) [c]
          go2 b (i, (Finished c b' r)) cs    = let b'' = fromIntegral b'
                                               in Right (r, b, b+b'') : go2 b'' (i, i) (c:cs)
          dup :: a -> (a, a)
          dup x = (x, x)
