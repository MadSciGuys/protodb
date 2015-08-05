{-|
Moddule      : ProtoDB.Serialization
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

import Data.Sequence (Seq)

import qualified Data.MultiSet as MS

import Data.ProtoBlob

import qualified Data.ByteString.Lazy.Char8 as B

import ProtoDB.Types.ProtoBinary
import ProtoDB.Types.ProtoInt
import ProtoDB.Types.ProtoReal
import ProtoDB.Types.ProtoString
import ProtoDB.Types.ProtoType
import ProtoDB.Types.ProtoField
import ProtoDB.Types.ProtoBinary

import ProtoDB.Parser

import Effusion.Data (chunk)

import Text.ProtocolBuffers.Basic (toUtf8)

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

data CellTypeGuess = CellTypeGuess {
    cellInt    :: !Int
  , cellReal   :: !Int
  , cellString :: !Int
  , cellBinary :: !Int
  } deriving (Eq, Show)

initCellTypeGuess = CellTypeGuess 0 0 0 0

tallyGuess :: Maybe ParseCellType -> CellTypeGuess -> CellTypeGuess
tallyGuess Nothing                ctg                     = ctg
tallyGuess (Just ParseIntType)    (CellTypeGuess i r s b) = CellTypeGuess (1+i) r s b
tallyGuess (Just ParseRealType)   (CellTypeGuess i r s b) = CellTypeGuess i (1+r) s b
tallyGuess (Just ParseStringType) (CellTypeGuess i r s b) = CellTypeGuess i r (1+s) b
tallyGuess (Just ParseBinaryType) (CellTypeGuess i r s b) = CellTypeGuess i r s (1+b)

finalGuess :: CellTypeGuess -> Maybe ParseCellType
finalGuess (CellTypeGuess i r s b) = if (i == r) && (r == s) && (s == b)
    then Nothing
    else Just $ (fst . head . sortBy (\(_, a) (_, b) -> compare a b)) cs
    where cs = [ (ParseIntType,    i)
               , (ParseRealType,   r)
               , (ParseStringType, s)
               , (ParseBinaryType, b)
               ]

firstRowHeuristic :: [B.ByteString] -> [Maybe ParseCellType]
firstRowHeuristic = map ((cellType <$>) . parseForType)
    where parseForType bs = if B.null bs then Nothing
                                         else (AL.maybeResult . AL.parse parseCell) bs

tallyRowHeuristic' :: [[B.ByteString]] -> [Maybe ParseCellType]
tallyRowHeuristic' (r:rs) = let l              = length r
                                ins ts bs      = (seqList ts) `seq` zipWith tallyGuess (firstRowHeuristic bs) ts
                                e              = replicate l initCellTypeGuess
                                seqList []     = []
                                seqList (x:xs) = x `seq` seqList xs
                           in map finalGuess $ foldl' ins e (r:rs)

tallyRowHeuristic :: [[B.ByteString]] -> [Maybe ParseCellType]
tallyRowHeuristic (r:rs) = let l   = length r
                               ins = zipWith tallyGuess . firstRowHeuristic
                               e   = replicate l initCellTypeGuess
                           in map finalGuess $ foldr ins e (r:rs)

tallyRowHeuristicPar :: Int -> [[B.ByteString]] -> [Maybe ParseCellType]
tallyRowHeuristicPar c (r:rs) = let l              = length r
                                    ins ts cs      = (seqList ts) `seq` zipWith tallyGuess cs ts
                                    e              = replicate l initCellTypeGuess
                                    seqList []     = []
                                    seqList (x:xs) = x `seq` seqList xs
                           in map finalGuess $ foldl' ins e $ (map firstRowHeuristic (r:rs) `using` parBuffer c (evalList rdeepseq))

lenParseAnyCellPutM :: ParseCell -> PutBlob
lenParseAnyCellPutM (ParseInt    i) = lenMessageUnsafePutM i
lenParseAnyCellPutM (ParseReal   r) = lenMessageUnsafePutM r
lenParseAnyCellPutM (ParseString s) = lenMessageUnsafePutM s
lenParseAnyCellPutM (ParseBinary b) = lenMessageUnsafePutM b
