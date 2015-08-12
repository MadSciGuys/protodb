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
  ) where

import ProtoDB.Types.ProtoBinary
import ProtoDB.Types.ProtoInt
import ProtoDB.Types.ProtoReal
import ProtoDB.Types.ProtoString
import ProtoDB.Types.ProtoDateType
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
