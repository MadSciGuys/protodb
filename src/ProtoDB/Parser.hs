{-|
Module      : ProtoDB.Parser
Description : ProtoDB Parsers
Copyright   : Travis Whitaker 2015
License     : MIT
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

This module provides parsers for generating ProtoDB records from other data
sources.
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, LambdaCase #-}

module ProtoDB.Parser where

import Control.Applicative

import Control.DeepSeq

import GHC.Generics (Generic)

import Data.Int

import Data.Digits (digitsRev)

import ProtoDB.Types.ProtoBinary
import ProtoDB.Types.ProtoInt
import ProtoDB.Types.ProtoReal
import ProtoDB.Types.ProtoString
import ProtoDB.Types.ProtoDateTime
import ProtoDB.Types.ProtoType
import ProtoDB.Types.ProtoField
import ProtoDB.Types.ProtoBinary

import Text.ProtocolBuffers.Basic (toUtf8)

import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A

data ParseCell = ParseInt    ProtoInt
               | ParseReal   ProtoReal
               | ParseString ProtoString
               | ParseBinary ProtoBinary
               deriving (Eq, Show)

data ParseCellType = ParseIntType
                   | ParseRealType
                   | ParseStringType
                   | ParseBinaryType
                   deriving (Eq, Ord, Show, Generic, NFData)

cellType :: ParseCell -> ParseCellType
cellType (ParseInt    _) = ParseIntType
cellType (ParseReal   _) = ParseRealType
cellType (ParseString _) = ParseStringType
cellType (ParseBinary _) = ParseBinaryType

parseProtoInt :: A.Parser ProtoInt
parseProtoInt = (ProtoInt . Just) <$> (A.decimal <* A.endOfInput)

parseProtoReal :: A.Parser ProtoReal
parseProtoReal = (ProtoReal . Just) <$> A.choice [noZero, A.double] <* A.endOfInput
    where noZero = do
            A.char '.'
            mts <- A.decimal
            let m = length (digitsRev 10 mts)
            return $ (fromIntegral mts) / (10 ^ m)

parseProtoString :: A.Parser ProtoString
parseProtoString = (ProtoString . Just) <$> (string <* A.endOfInput)
    where string = toUtf8 <$> A.takeLazyByteString >>=
            \case (Right u) -> return u
                  (Left i)  -> fail $ "UTF8 decoding failure at " ++ (show i)

parseProtoBinary :: A.Parser ProtoBinary
parseProtoBinary = (ProtoBinary . Just) <$> (A.takeLazyByteString <* A.endOfInput)

parseCell :: A.Parser ParseCell
parseCell = A.choice [ (ParseInt    <$> parseProtoInt)
                     , (ParseReal   <$> parseProtoReal)
                     , (ParseString <$> parseProtoString)
                     , (ParseBinary <$> parseProtoBinary)
                     ]

parseTypeCheck :: ParseCellType -> ParseCell -> Bool
parseTypeCheck ParseIntType (ParseInt       _) = True
parseTypeCheck ParseRealType (ParseReal     _) = True
parseTypeCheck ParseStringType (ParseString _) = True
parseTypeCheck ParseBinaryType (ParseBinary _) = True
parseTypeCheck _               _               = False
