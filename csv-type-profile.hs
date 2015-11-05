{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BLC

import ProtoDB.Parser (tallyRowHeuristic')
import ProtoDB.Types (ProtoCellType)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  (n:paths) -> mapM_ (\ path 
      -> writeFile (path++".PDB.profile") . show . onFile (read n) 
        =<< BLC.readFile path
    ) paths
  _ -> putStrLn $ "USEAGE: \n"
    ++"  #linesSampleSize list of input CSV files"

onFile :: Int -> BLC.ByteString -> [Maybe ProtoCellType]
onFile sampleSize = tallyRowHeuristic' . map (BLC.split ',') 
  . (if sampleSize > 0 then take sampleSize else id) . BLC.lines
