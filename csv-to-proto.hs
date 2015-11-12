{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T

import Control.Monad (liftM2)
import Data.Attoparsec.ByteString.Lazy (eitherResult, parse)
import Data.Binary.Put (Put, runPut)
import Data.Int (Int64)
import Data.List (uncons)
import System.IO (IOMode(WriteMode), withFile)
import System.Environment (getArgs)

import CSVChunk (foldMapChunksFile, mapChunksFile)
import Data.ProtoBlob (lenMessagePutM)

import ProtoDB.Parser (
    tallyRowHeuristic', tallyRows, CellBlock(..)
  , attemptDecode, finalGuess, parseCSVBody
  )
import ProtoDB.Types (ProtoCellType(ProtoStringType))
import ProtoDB.Writer (WritableField(..), putProtoCell, mkProtoDB, expandTypes)

errSelf :: String -> a
errSelf = error . ("csv-type-profile.hs."++)

defChunkSize :: Int64
defChunkSize = 10*1024*1024

profileSuffix :: String
profileSuffix = ".PDB.profile"

rewriteSuffix :: String
rewriteSuffix = ".PDB.protoblob"

main :: IO ()
main = getArgs >>= \case
  (('-':'-':'p':_):paths) -> mapM_ (\ path 
      -> writeFile (path++profileSuffix) . show 
        =<< profileChunks defChunkSize path
    ) paths
  (('-':'-':'n':_):n:paths) -> mapM_ (\ path
      -> writeFile (path++profileSuffix) . show . profileN (read n) 
        =<< BLC.readFile path
    ) paths
  (('-':'-':'r':_):paths) -> mapM_ reWriteFile paths
  _ -> putStrLn $ "USEAGE: \n"
    ++"  --profile list of input CSV files \n"
    ++"  --n #linesSample list of input CSV files \n"
    ++"  --rewrite datablockName.csv \n"
    ++"(rewrite mode must be done on a file which has already had --profile run on it)"

reWriteFile :: FilePath -> IO ()
reWriteFile csv = withFile (csv++rewriteSuffix) WriteMode $ \ out -> do
  flds <- read <$> readFile (csv++profileSuffix)
  BLC.hPut out $ runPut $ lenMessagePutM $ mkProtoDB (T.pack csv) flds
  mapChunksFile defChunkSize csv out $ \ _ -> reWriteChunk' $ expandTypes flds

reWriteChunk' :: [ProtoCellType] -> BC.ByteString -> BC.ByteString
reWriteChunk' profile 
  = BLC.toStrict . runPut . reWriteChunk profile . BLC.fromStrict

reWriteChunk :: [ProtoCellType] -> BLC.ByteString -> Put
reWriteChunk profile = mapM_ putProtoCell
  . either (error . ("csv-type-profile.hs.reWriteChunk: parse error: \n"++)) id
  . eitherResult . parse (parseCSVBody profile)

profileChunks :: Int64 -> FilePath -> IO [WritableField]
profileChunks chunkSize path = liftM2 (zipWith $ \ hdrFld fldType 
    -> WritableField (attemptDecode hdrFld) fldType []
  )
  (BLC.split ',' . BLC.copy . fst 
    . maybe (errSelf $ "onFileChunks: file "++path++" is empty") id
    . uncons . BLC.lines <$> BLC.readFile path
  )
  (fmap (map (maybe ProtoStringType id . finalGuess) . openCellBlock)
    $ foldMapChunksFile chunkSize path $ \ _
    ->CellBlock . tallyRows . map (BLC.split ',') . BLC.lines . BLC.fromStrict
  )

profileN :: Int -> BLC.ByteString -> [Maybe ProtoCellType]
profileN sampleLines = tallyRowHeuristic' . map (BLC.split ',')
  . (if sampleLines > 0 then take sampleLines else id) . BLC.lines
