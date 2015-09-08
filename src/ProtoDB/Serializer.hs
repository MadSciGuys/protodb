{-|
Module      : ProtoDB.Serialization
Description : ProtoDB Data Block Serializer
Copyright   : Travis Whitaker 2015
License     : MIT
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

This module provides a comprehensive type for representing a data block and
functions for serialization, principally intended for converting CSV files.
-}

{-# LANGUAGE LambdaCase, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ProtoDB.Serializer (
    putCollection
  , putArray
  , tare1, tare2, tare3, tare4
  , putCell )
where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Sequence as Q

import Data.Array.IArray
import Data.Ix
import Text.ProtocolBuffers.Basic (Utf8(..))

import Data.ProtoBlob (PutBlob, lenMessagePutM)
import Data.Word (Word32)

import ProtoDB.Types (ProtoCell(..), ProtoCellType)
import ProtoDB.Types.ProtoField (ProtoField(..)) 
import ProtoDB.Types.ProtoType (ProtoType(..))
import ProtoDB.Types.ProtoInt (ProtoInt(..))
import ProtoDB.Types.ProtoReal (ProtoReal(..))
import ProtoDB.Types.ProtoString (ProtoString(..))
import ProtoDB.Types.ProtoBinary (ProtoBinary(..))

fI :: (Integral n, Num x) => n -> x
fI = fromIntegral

putCollection :: (Foldable f)
  => BLC.ByteString -> ProtoType -> f ProtoCell -> PutBlob
putCollection title pcType
  = putArray title pcType ((:[]) . fromIntegral . length)

putArray :: (Foldable f)
  => BLC.ByteString 
  -> ProtoType 
  -> (f ProtoCell -> [Word32])
  -> f ProtoCell 
  -> PutBlob
putArray title pcType toVecShape cells = do
  lenMessagePutM
    $ ProtoField (Utf8 title) pcType (Q.fromList $ toVecShape cells)
  mapM_ putCell cells

tare1 :: (Ix i, Integral n) => Array i a -> [n]
tare1 = (\ ((xl), (xr)) -> [fI $ rangeSize (xl, xr)] ) . bounds

tare2 :: (Ix i, Integral n) => Array (i, i) a -> [n]
tare2 = (\ ((xl, yl), (xr, yr)) 
    -> map (fI . rangeSize) [(xl, xr), (yl, yr)]
  ) . bounds

tare3 :: (Ix i, Integral n) => Array (i, i, i) a -> [n]
tare3 = (\ ((xl, yl, zl), (xr, yr, zr))
    -> map (fI . rangeSize) [(xl, xr), (yl, yr), (zl, zr)]
  ) . bounds

tare4 :: (Ix i, Integral n) => Array (i, i, i, i) a -> [n]
tare4 = (\ ((wl, xl, yl, zl), (wr, xr, yr, zr))
    -> map (fI . rangeSize) [(wl, wr), (xl, xr), (yl, yr), (zl, zr)]
  ) . bounds

putCell :: ProtoCell -> PutBlob
putCell = \case
  ProtoIntCell      v -> p v
  ProtoRealCell     v -> p v
  ProtoStringCell   v -> p v
  ProtoDateTimeCell v -> p v
  ProtoBinaryCell   v -> p v
  where p = lenMessagePutM
