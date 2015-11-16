{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, LambdaCase, NoMonomorphismRestriction #-}

import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.Text.Encoding (encodeUtf8)
import Text.ProtocolBuffers.Basic(Utf8(..))
import Text.ProtocolBuffers.WireMessage (runGetOnLazy)
import System.Environment (getArgs)

import ProtoDB.Reader (ReadDB(..), ReadField(rfTitle), lazyForceReadDB)
import ProtoDB.Types (ProtoCell(..), ProtoInt(..), ProtoReal(..)
  , ProtoString(..), ProtoBinary(..), ProtoDateTime(..)
  )

main :: IO ()
main = getArgs >>= \ paths -> flip mapM_ paths $ \ path -> do 
  {-
  (((ReadDB _ flds _), rows), _)
    <- either error id . runGetOnLazy forceReadDB <$> BLC.readFile path
    -}
  ((ReadDB _ flds _), rows) <- lazyForceReadDB <$> BLC.readFile path
  line $ map (BLC.fromStrict . encodeUtf8 . rfTitle) flds
  flip mapM_ rows $ line . map dumpCell
  where line = BLC.putStrLn . BLC.intercalate ","

--help; I'm using strings D:
dumpCell :: ProtoCell -> BLC.ByteString
dumpCell = \case
  ProtoIntCell      (ProtoInt mbn)        -> blank tShow              mbn
  ProtoRealCell     (ProtoReal mbx)       -> blank tShow              mbx
  ProtoStringCell   (ProtoString mbutf8)  -> blank (\ (Utf8 s) -> s ) mbutf8
  ProtoDateTimeCell (ProtoDateTime mbt)   -> blank (const "TIME!")    mbt
  ProtoBinaryCell   (ProtoBinary mbs)     -> blank id                 mbs
  where blank = maybe (BLC.empty); tShow = BLC.pack . show
  
