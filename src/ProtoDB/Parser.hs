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
import Control.DeepSeq
import Control.Monad
import Data.Int
import Data.Digits (digitsRev)
import Data.List (foldl', sortBy)
import Data.Monoid ((<>), Monoid(..))
import GHC.Generics (Generic)
import Text.ProtocolBuffers.Basic (toUtf8)

import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy  as AL

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import ProtoDB.Writer
import Data.ProtoBlob
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8', decodeLatin1)

import ProtoDB.Types 

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
  } deriving (Eq, Show, Read, NFData, Generic)

initCellTypeGuess :: CellTypeGuess
initCellTypeGuess = CellTypeGuess 0 0 0 0 0

zipPad :: (Monoid m1, Monoid m2) => [m1] -> [m2] -> [(m1, m2)]
zipPad (m1:m1s) (m2:m2s) = (m1    , m2    ) : zipPad m1s m2s
zipPad (m1:m1s) []       = (m1    , mempty) : zipPad m1s []
zipPad []       (m2:m2s) = (mempty, m2    ) : zipPad []  m2s
zipPad [] [] = []

instance Monoid CellTypeGuess where
  mappend (CellTypeGuess i1 r1 s1 d1 b1) (CellTypeGuess i2 r2 s2 d2 b2) 
    = CellTypeGuess (i1+i2) (r1+r2) (s1+s2) (d1+d2) (b1+b2)
  mempty = initCellTypeGuess

newtype CellBlock = CellBlock { openCellBlock :: [CellTypeGuess] }
  deriving (Show, Read, NFData, Generic)

instance Monoid CellBlock where
  mappend (CellBlock ctgs1) (CellBlock ctgs2)
    = CellBlock $ map (uncurry mappend) $ zipPad ctgs1 ctgs2
  mempty = CellBlock []

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

-- | "Guess" the field types of a sample of datablock records. 
--   This function is lazy on the input list of bytestrings; 
--   use it for large or streaming data sets.
tallyRowHeuristic :: [[B.ByteString]] -> [Maybe ProtoCellType]
tallyRowHeuristic (r:rs) = let l   = length r
                               ins = zipWith tallyGuess . firstRowHeuristic
                               e   = replicate l initCellTypeGuess
                           in map finalGuess $ foldr ins e (r:rs)

-- | Strict implementation of 'tallyRowHeuristic'. This function is strict; use
--   it for large data sets.
tallyRowHeuristic' :: [[B.ByteString]] -> [Maybe ProtoCellType]
tallyRowHeuristic' = map finalGuess . tallyRows

tallyRows :: [[B.ByteString]] -> [CellTypeGuess]
tallyRows (r:rs) = let ins ts bs      = (seqList ts) `seq` zipWith tallyGuess (firstRowHeuristic bs) ts
                       e              = replicate (length r) initCellTypeGuess
                       seqList []     = []
                       seqList (x:xs) = x `seq` seqList xs
                  in foldl' ins e (r:rs)

attemptDecode :: BL.ByteString -> T.Text
attemptDecode bStr = toStrict $ either (const $ decodeLatin1 bStr) id $ decodeUtf8' bStr

-- | Given the title and contents of a Datablock as a CSV, return either an
--   error or the contents protocol buffer Datablock.
csvToProto :: T.Text -> BL.ByteString -> Either String BL.ByteString
csvToProto title csv = runPutBlob . writeDB 
  <$> ( WritableDB <$> pure title <*> fields 
          <*> (AL.eitherResult . flip AL.parse csv =<< parseCSV <$> tys)
      )
    where
        rows :: [[BL.ByteString]]
        rows = map (B.split ',') $ B.lines csv

        -- | WARNING: 'titles' involves converting a lazy Text to a strict Text.
        titles :: [T.Text]
        titles = map attemptDecode $ head rows

        tys :: Either String [ProtoCellType]
        tys = mapM (maybe (Left "Unable to guess type of CSV column.") Right) (tallyRowHeuristic' $ tail rows)

        fields :: Either String [WritableField]
        fields = map ($ []) . map (uncurry WritableField) . zip titles <$> tys

-- | Parse a complete CSV document, given the types of each column.
parseCSV :: [ProtoCellType] -> AL.Parser [ProtoCell]
parseCSV ts = do
    AL.manyTill A.anyChar A.endOfLine
    parseCSVBody ts

-- | Parse the body of a CSV document, omitting a title header, given the types
--   of each column.
parseCSVBody :: [ProtoCellType] -> AL.Parser [ProtoCell]
parseCSVBody pcts = concat <$> AL.manyTill row AL.endOfInput
  where
    isDelim c = c == '\n' || c == '\r' || c == ','
    wrapParser valWrapper nothingWrapper parser 
      = fmap valWrapper $ parser isDelim <|> return (nothingWrapper Nothing)
    row :: AL.Parser [ProtoCell]
    row = flip mapM pcts $ \case
      ProtoIntType      -> wrapParser ProtoIntCell      ProtoInt      parseProtoIntD     
      ProtoRealType     -> wrapParser ProtoRealCell     ProtoReal     parseProtoRealD    
      ProtoDateTimeType -> wrapParser ProtoDateTimeCell ProtoDateTime parseProtoDateTimeD
      ProtoStringType   -> wrapParser ProtoStringCell   ProtoString   parseProtoStringD  
      ProtoBinaryType   -> wrapParser ProtoBinaryCell   ProtoBinary   parseProtoBinaryD  
