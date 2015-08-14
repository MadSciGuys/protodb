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

import Control.Monad

import Control.DeepSeq

import GHC.Generics (Generic)

import Data.Int

import Data.Digits (digitsRev)

import Data.List (foldl', sortBy)

import ProtoDB.Types

import Text.ProtocolBuffers.Basic (toUtf8)

import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy  as AL

import qualified Data.ByteString.Lazy.Char8 as B

emptyCell :: (Maybe a -> b) -> A.Parser b
emptyCell c = A.endOfInput >> return (c Nothing)

parseProtoInt :: A.Parser ProtoInt
parseProtoInt = emptyCell ProtoInt <|> (ProtoInt . Just) <$> (A.decimal <* A.endOfInput)

parseProtoReal :: A.Parser ProtoReal
parseProtoReal = emptyCell ProtoReal <|> (ProtoReal . Just) <$> A.choice [noZero, A.double] <* A.endOfInput
    where noZero = do
            A.char '.'
            mts <- A.decimal
            let m = length (digitsRev 10 mts)
            return $ (fromIntegral mts) / (10 ^ m)

parseProtoString :: A.Parser ProtoString
parseProtoString = emptyCell ProtoString <|> (ProtoString . Just) <$> (string <* A.endOfInput)
    where string = toUtf8 <$> A.takeLazyByteString >>=
            \case (Right u) -> return u
                  (Left i)  -> fail $ "UTF8 decoding failure at " ++ (show i)

-- | Please help.
parseProtoDateTime :: A.Parser ProtoDateTime
parseProtoDateTime = emptyCell ProtoDateTime <|> fail "Date/Time parser not implemented. Please help."

parseProtoBinary :: A.Parser ProtoBinary
parseProtoBinary = emptyCell ProtoBinary <|> (ProtoBinary . Just) <$> (A.takeLazyByteString <* A.endOfInput)

parseProtoCell :: A.Parser (Maybe ProtoCell)
parseProtoCell = (A.endOfInput >> return Nothing) <|> Just <$>
    A.choice [ (ProtoIntCell      <$> parseProtoInt)
             , (ProtoRealCell     <$> parseProtoReal)
             , (ProtoDateTimeCell <$> parseProtoDateTime)
             , (ProtoStringCell   <$> parseProtoString)
             , (ProtoBinaryCell   <$> parseProtoBinary)
             ]

data CellTypeGuess = CellTypeGuess {
    cellInt      :: !Int
  , cellReal     :: !Int
  , cellString   :: !Int
  , cellDateTime :: !Int
  , cellBinary   :: !Int
  } deriving (Eq, Show)

initCellTypeGuess = CellTypeGuess 0 0 0 0 0

tallyGuess :: Maybe ProtoCellType -> CellTypeGuess -> CellTypeGuess
tallyGuess Nothing                  ctg                       = ctg
tallyGuess (Just ProtoIntType)      (CellTypeGuess i r s d b) = CellTypeGuess (1+i) r s d b
tallyGuess (Just ProtoRealType)     (CellTypeGuess i r s d b) = CellTypeGuess i (1+r) s d b
tallyGuess (Just ProtoStringType)   (CellTypeGuess i r s d b) = CellTypeGuess i r (1+s) d b
tallyGuess (Just ProtoDateTimeType) (CellTypeGuess i r s d b) = CellTypeGuess i r s (1+d) b
tallyGuess (Just ProtoBinaryType)   (CellTypeGuess i r s d b) = CellTypeGuess i r s d (1+b)

finalGuess :: CellTypeGuess -> Maybe ProtoCellType
finalGuess (CellTypeGuess i r s d b) = if (i == r) && (r == s) && (s == d) && (d == b)
    then Nothing
    else Just $ (fst . head . sortBy (\(_, a) (_, b) -> compare a b)) cs
    where cs = [ (ProtoIntType,      i)
               , (ProtoRealType,     r)
               , (ProtoStringType,   s)
               , (ProtoDateTimeType, d)
               , (ProtoBinaryType,   b)
               ]

firstRowHeuristic :: [B.ByteString] -> [Maybe ProtoCellType]
firstRowHeuristic = map ((protoCellType <$>) . parseForType)
    where parseForType bs = (join . AL.maybeResult . AL.parse parseProtoCell) bs

tallyRowHeuristic' :: [[B.ByteString]] -> [Maybe ProtoCellType]
tallyRowHeuristic' (r:rs) = let l              = length r
                                ins ts bs      = (seqList ts) `seq` zipWith tallyGuess (firstRowHeuristic bs) ts
                                e              = replicate l initCellTypeGuess
                                seqList []     = []
                                seqList (x:xs) = x `seq` seqList xs
                           in map finalGuess $ foldl' ins e (r:rs)

tallyRowHeuristic :: [[B.ByteString]] -> [Maybe ProtoCellType]
tallyRowHeuristic (r:rs) = let l   = length r
                               ins = zipWith tallyGuess . firstRowHeuristic
                               e   = replicate l initCellTypeGuess
                           in map finalGuess $ foldr ins e (r:rs)
