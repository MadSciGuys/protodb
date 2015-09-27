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
    WritableField(..)
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
  } deriving (Eq, Ord, Show)

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
toProtoDB (WritableDB t fs _) = ProtoDB (textToUtf8 t) (fromIntegral (length fs))

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

writeDB :: WritableDB -> PutBlob
writeDB w@(WritableDB _ fs cs) = pdb >> pfs >> pcs
    where pdb = lenMessagePutM (toProtoDB w)
          pfs = mapM_ (lenMessagePutM . toProtoField) fs
          --ts  = map protoCellTypeProtoType fs
          pcs = putRow fs cs
          putRow []      []                            = return ()
          putRow []      cs                            = putRow fs cs
          putRow ((WritableField _ pct []):ts') (c:cs) = putCell pct c >> putRow ts' cs
          putRow ((WritableField _ pct vs):ts') cs     = (mapM_ (putCell pct) $ take (product vs) cs)
                                                       >> putRow ts' cs
          putCell pct c = if pct `protoTypeMatch` c
                          then putProtoCell c
                          else error "writeDB: type mismatch."
