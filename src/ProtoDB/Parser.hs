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

All parsers are expected to consume an entire "cell," i.e. must consume all of
their input.
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

import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy  as AL

import qualified Data.ByteString.Lazy.Char8 as B

-- | Parse an empty cell as "missing data," i.e. 'Nothing' inside of a ProtoDB
--   type constructor.
emptyCell :: (Maybe a -> b) -- ^ External type constructor.
          -> A.Parser b
emptyCell c = A.endOfInput >> return (c Nothing) <?> "emptyCell"

-- | Parse an integer cell. The empty string is recognized as "missing data,"
--   otherwise the parser has the same behavior as Attoparsec's 'A.decimal'.
parseProtoInt :: A.Parser ProtoInt
parseProtoInt = emptyCell ProtoInt <|> (ProtoInt . Just) <$> (A.decimal <* A.endOfInput) <?> "parseProtoInt"

-- | Parse a cell containing a real number. The empty string is recognized as
--   "missing data," otherwise the parser has the same behavior as Attoparsec's
--   'A.double', with the exption of allowing numbers between zero and one to be
--   written without a leading zero digit. For example, 'A.double' would accept
--   "0.123" but reject ".123" while this function accepts both.
parseProtoReal :: A.Parser ProtoReal
parseProtoReal = emptyCell ProtoReal <|> (ProtoReal . Just) <$> A.choice [noZero, A.double] <* A.endOfInput <?> "parseProtoReal"
    where noZero = do
            A.char '.'
            mts <- A.decimal
            let m = length (digitsRev 10 mts)
            return $ (fromIntegral mts) / (10 ^ m)

-- | Parse a cell containing a string. The empty string is recognized as
--   "missing data," otherwise the parser accepts all valid UTF8 strings. Note
--   that arbitrary binary data may coincidentally constitute a valid UTF8
--   string; make sure that the semantics of 'ProtoString' and 'ProtoBinary'
--   are being appropriately applied.
parseProtoString :: A.Parser ProtoString
parseProtoString = emptyCell ProtoString <|> (ProtoString . Just) <$> (string <* A.endOfInput) <?> "parseProtoString"
    where string = toUtf8 <$> A.takeLazyByteString >>=
            \case (Right u) -> return u
                  (Left i)  -> fail $ "UTF8 decoding failure at " ++ (show i)

-- | Please help.
parseProtoDateTime :: A.Parser ProtoDateTime
parseProtoDateTime = emptyCell ProtoDateTime <|> fail "Date/Time parser not implemented. Please help." <?> "parseProtoDateTime"

-- | Parse a cell containing arbitrary binary data. The empty string is
--   recognized as "missing data," otherwise the parser accepts any non-empty
--   string.
parseProtoBinary :: A.Parser ProtoBinary
parseProtoBinary = emptyCell ProtoBinary <|> (ProtoBinary . Just) <$> (A.takeLazyByteString <* A.endOfInput) <?> "parseProtoBinary"

-- | This function parses a cell whose type is unknown. No type can be inferred
--   from the empty string alone, so 'Nothing' is returned in this case. Parsers
--   are tried in order, from least-accepting to most-accepting:
--
--   1. Integer
--   2. Real
--   3. Date/Time
--   4. String (accepts any valid UTF8 string)
--   5. Binary (accepts any binary data)
parseProtoCell :: A.Parser (Maybe ProtoCell)
parseProtoCell = (A.endOfInput >> return Nothing) <|> Just <$>
    A.choice [ (ProtoIntCell      <$> parseProtoInt)
             , (ProtoRealCell     <$> parseProtoReal)
             , (ProtoDateTimeCell <$> parseProtoDateTime)
             , (ProtoStringCell   <$> parseProtoString)
             , (ProtoBinaryCell   <$> parseProtoBinary)
             ] <?> "parseProtoCell"

-- | Parse an empty cell as "missing data," i.e. 'Nothing' inside of a ProtoDB
--   type constructor.
emptyCellD :: (Maybe a -> b) -- ^ External type constructor.
           -> (Char -> Bool) -- ^ Returns 'True' when delimiter is detected.
           -> A.Parser b
emptyCellD c p = A.satisfy p >> return (c Nothing) <?> "emptyCellD"

-- | Parse an integer cell. The empty string is recognized as "missing data,"
--   otherwise the parser has the same behavior as Attoparsec's 'A.decimal'.
parseProtoIntD :: (Char -> Bool) -- ^ Returns 'True' when delimiter is detected.
               -> A.Parser ProtoInt
parseProtoIntD p = emptyCellD ProtoInt p <|> (ProtoInt . Just) <$> (A.decimal <* A.satisfy p) <?> "parseProtoIntD"

-- | Parse a cell containing a real number. The empty string is recognized as
--   "missing data," otherwise the parser has the same behavior as Attoparsec's
--   'A.double', with the exption of allowing numbers between zero and one to be
--   written without a leading zero digit. For example, 'A.double' would accept
--   "0.123" but reject ".123" while this function accepts both.
parseProtoRealD :: (Char -> Bool) -- ^ Returns 'True' when delimiter is detected.
                -> A.Parser ProtoReal
parseProtoRealD p = emptyCellD ProtoReal p <|> (ProtoReal . Just) <$> A.choice [noZero, A.double] <* A.satisfy p <?> "parseProtoRealD"
    where noZero = do
            A.char '.'
            mts <- A.decimal
            let m = length (digitsRev 10 mts)
            return $ (fromIntegral mts) / (10 ^ m)

-- | Parse a cell containing a string. The empty string is recognized as
--   "missing data," otherwise the parser accepts all valid UTF8 strings. Note
--   that arbitrary binary data may coincidentally constitute a valid UTF8
--   string; make sure that the semantics of 'ProtoString' and 'ProtoBinary'
--   are being appropriately applied.
parseProtoStringD :: (Char -> Bool) -- ^ Returns 'True' when delimiter is detected.
                  -> A.Parser ProtoString
parseProtoStringD p = emptyCellD ProtoString p <|> (ProtoString . Just) <$> (string <* A.satisfy p) <?> "parseProtoStringD"
    where string = (toUtf8 . B.fromStrict) <$> A.takeTill p >>=
            \case (Right u) -> return u
                  (Left i)  -> fail $ "UTF8 decoding failure at " ++ (show i)

-- | Please help.
parseProtoDateTimeD :: (Char -> Bool) -- ^ Returns 'True' when delimiter is detected.
                    -> A.Parser ProtoDateTime
parseProtoDateTimeD p = emptyCellD ProtoDateTime p <|> fail "Date/Time parser not implemented. Please help." <?> "parseProtoDateTimeD"

-- | Parse a cell containing arbitrary binary data. The empty string is
--   recognized as "missing data," otherwise the parser accepts any non-empty
--   string.
parseProtoBinaryD :: (Char -> Bool) -- ^ Returns 'True' when delimiter is detected.
                  -> A.Parser ProtoBinary
parseProtoBinaryD p = emptyCellD ProtoBinary p <|> (ProtoBinary . Just . B.fromStrict) <$> (A.takeTill p <* A.satisfy p) <?> "parseProtoBinaryD"

-- | This function parses a cell whose type is unknown. No type can be inferred
--   from the empty string alone, so 'Nothing' is returned in this case. Parsers
--   are tried in order, from least-accepting to most-accepting:
--
--   1. Integer
--   2. Real
--   3. Date/Time
--   4. String (accepts any valid UTF8 string)
--   5. Binary (accepts any binary data)
parseProtoCellD :: (Char -> Bool) -- ^ Returns 'True' when delimiter is detected.
                -> A.Parser (Maybe ProtoCell)
parseProtoCellD p = (A.satisfy p >> return Nothing) <|> Just <$>
    A.choice [ (ProtoIntCell      <$> parseProtoIntD p)
             , (ProtoRealCell     <$> parseProtoRealD p)
             , (ProtoDateTimeCell <$> parseProtoDateTimeD p)
             , (ProtoStringCell   <$> parseProtoStringD p)
             , (ProtoBinaryCell   <$> parseProtoBinaryD p)
             ] <?> "parseProtoCellD"

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
    else Just $ (fst . head . sortBy (\(_, a) (_, b) -> compare b a)) cs
    where cs = [ (ProtoIntType,      i)
               , (ProtoRealType,     r)
               , (ProtoStringType,   s)
               , (ProtoDateTimeType, d)
               , (ProtoBinaryType,   b)
               ]

-- | "Guess" the type of a list of cells representing a single datablock record.
firstRowHeuristic :: [B.ByteString] -> [Maybe ProtoCellType]
firstRowHeuristic = map ((protoCellType <$>) . parseForType)
    where parseForType bs = (join . AL.maybeResult . AL.parse parseProtoCell) bs

-- | "Guess" the field types of a sample of datablock records. This function is
--   lazy; use it for small or streaming data sets.
tallyRowHeuristic :: [[B.ByteString]] -> [Maybe ProtoCellType]
tallyRowHeuristic (r:rs) = let l   = length r
                               ins = zipWith tallyGuess . firstRowHeuristic
                               e   = replicate l initCellTypeGuess
                           in map finalGuess $ foldr ins e (r:rs)

-- | Strict implementation of 'tallyRowHeuristic'. This function is strict; use
--   it for large data sets.
tallyRowHeuristic' :: [[B.ByteString]] -> [Maybe ProtoCellType]
tallyRowHeuristic' (r:rs) = let l              = length r
                                ins ts bs      = (seqList ts) `seq` zipWith tallyGuess (firstRowHeuristic bs) ts
                                e              = replicate l initCellTypeGuess
                                seqList []     = []
                                seqList (x:xs) = x `seq` seqList xs
                           in map finalGuess $ foldl' ins e (r:rs)

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
