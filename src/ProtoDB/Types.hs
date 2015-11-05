{-|
Module      : ProtoDB.Types
Description : ProtoDB Parsers
Copyright   : Travis Whitaker 2015
License     : MIT
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, LambdaCase #-}

module ProtoDB.Types (
    ProtoType(..)
  , ProtoField(..)
  , ProtoDB(..)
  , ProtoBinary(..)
  , ProtoString(..)
  , ProtoDateTime(..)
  , ProtoReal(..)
  , ProtoInt(..)
  , ProtoCell(..)
  , ProtoCellType(..)
  , protoCellType
  , protoCellTypeProtoType
  , protoTypeProtoCellType
  , protoTypeMatch
  ) where

import ProtoDB.Types.ProtoInt
import ProtoDB.Types.ProtoReal
import ProtoDB.Types.ProtoString
import ProtoDB.Types.ProtoDateTime
import ProtoDB.Types.ProtoType
import ProtoDB.Types.ProtoField
import ProtoDB.Types.ProtoDB
import ProtoDB.Types.ProtoBinary

-- | Polymorphic datablock cell type.
data ProtoCell = ProtoIntCell      ProtoInt
               | ProtoRealCell     ProtoReal
               | ProtoStringCell   ProtoString
               | ProtoDateTimeCell ProtoDateTime
               | ProtoBinaryCell   ProtoBinary
               deriving (Eq, Ord, Show)

-- | This type has a subtly different role than 'ProtoType'; it's intended to be
--   used for parser/serializer selection, rather than field type indication.
data ProtoCellType = ProtoIntType
                   | ProtoRealType
                   | ProtoStringType
                   | ProtoDateTimeType
                   | ProtoBinaryType
                   deriving (Eq, Ord, Show, Read)

protoCellType :: ProtoCell -> ProtoCellType
protoCellType (ProtoIntCell _)      = ProtoIntType
protoCellType (ProtoRealCell _)     = ProtoRealType
protoCellType (ProtoStringCell _)   = ProtoStringType
protoCellType (ProtoDateTimeCell _) = ProtoDateTimeType
protoCellType (ProtoBinaryCell _)   = ProtoBinaryType

-- | Please give this a better name.
protoCellTypeProtoType :: ProtoCellType -> ProtoType
protoCellTypeProtoType ProtoIntType      = Int
protoCellTypeProtoType ProtoRealType     = Real
protoCellTypeProtoType ProtoStringType   = String
protoCellTypeProtoType ProtoDateTimeType = DateTime
protoCellTypeProtoType ProtoBinaryType   = Binary

-- | Please give this a better name.
protoTypeProtoCellType :: ProtoType -> ProtoCellType
protoTypeProtoCellType Int      = ProtoIntType
protoTypeProtoCellType Real     = ProtoRealType
protoTypeProtoCellType String   = ProtoStringType
protoTypeProtoCellType DateTime = ProtoDateTimeType
protoTypeProtoCellType Binary   = ProtoBinaryType

protoTypeMatch :: ProtoCellType -> ProtoCell -> Bool
protoTypeMatch ProtoIntType      (ProtoIntCell _)        = True
protoTypeMatch ProtoIntType      _                       = False
protoTypeMatch ProtoRealType     (ProtoRealCell _)       = True
protoTypeMatch ProtoRealType     _                       = False
protoTypeMatch ProtoStringType   (ProtoStringCell _)     = True
protoTypeMatch ProtoStringType   _                       = False
protoTypeMatch ProtoDateTimeType (ProtoDateTimeCell _)   = True
protoTypeMatch ProtoDateTimeType _                       = False
protoTypeMatch ProtoBinaryType   (ProtoBinaryCell _)     = True
protoTypeMatch ProtoBinaryType   _                       = False
