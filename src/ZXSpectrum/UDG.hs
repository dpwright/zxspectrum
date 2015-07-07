{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities for working with UDGs (User-Defined Graphics)

module ZXSpectrum.UDG
  ( pattern UDG_LOC
  , udg
  ) where

import Z80
import Data.Bits
import Data.ByteString (pack)

pattern UDG_LOC = 0x5c7b :: Location

udg :: [String] -> Z80ASM
udg = defb . pack . map parseLine where
  parseChar ' ' = 0
  parseChar _   = 1
  parseLine l
    | length l /= 8 = error "Each line in a UDG must be 8 characters"
    | otherwise     = foldr (.|.) 0 $ zipWith shiftL (map parseChar l) [7,6..0]
