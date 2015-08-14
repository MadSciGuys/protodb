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

import Data.ProtoBlob

import qualified Data.ByteString.Lazy.Char8 as B

import ProtoDB.Types
import ProtoDB.Parser
