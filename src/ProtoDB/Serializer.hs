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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module ProtoDB.Serializer where

import Control.Applicative

import Control.Parallel.Strategies

import Data.Int

import Data.List

import Data.ProtoBlob

import qualified Data.ByteString.Lazy.Char8 as B

import ProtoDB.Types
import ProtoDB.Parser

import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy  as AL

data Field = Field {
    fieldTitle  :: B.ByteString
  , fieldType   :: ProtoType
  , vectorShape :: [Int]
  } deriving (Eq, Show)

data DataBlock = DataBlock {
    pdbTitle  :: B.ByteString
  , pdbFields :: [Field]
  , payload   :: [[B.ByteString]]
  } deriving (Eq, Show)

--tallyRowHeuristicPar :: Int -> [[B.ByteString]] -> [Maybe ProtoCellType]
--tallyRowHeuristicPar c (r:rs) = let l              = length r
--                                    ins ts cs      = (seqList ts) `seq` zipWith tallyGuess cs ts
--                                    e              = replicate l initCellTypeGuess
--                                    seqList []     = []
--                                    seqList (x:xs) = x `seq` seqList xs
--                           in map finalGuess $ foldl' ins e $ (map firstRowHeuristic (r:rs) `using` parBuffer c (evalList rdeepseq))

--lenParseAnyCellPutM :: ProtoCell -> PutBlob
--lenParseAnyCellPutM (ProtoInt      i) = lenMessageUnsafePutM i
--lenParseAnyCellPutM (ProtoReal     r) = lenMessageUnsafePutM r
--lenParseAnyCellPutM (ProtoString   s) = lenMessageUnsafePutM s
--lenParseAnyCellPutM (ProtoDateTime d) = lenMessageUnsafePutM d
--lenParseAnyCellPutM (ProtoBinary   b) = lenMessageUnsafePutM b
