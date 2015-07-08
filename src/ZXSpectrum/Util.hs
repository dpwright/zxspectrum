{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous utilities useful in ZX Spectrum development.

module ZXSpectrum.Util
  ( -- * Printing utilities
    chr
  , printA
  , printVal
  , setCursorPos
    -- * Metasequences
  , pattern INK
  , pattern PAPER
  , pattern AT
  ) where

import Z80
import Data.Word

pattern INK   = 0x10 :: Word8
pattern PAPER = 0x11 :: Word8
pattern AT    = 0x16 :: Word8

-- | Convert a "Char" to a "Word8"
chr :: Char -> Word8
chr = fromIntegral . fromEnum

-- | Print the value of whatever's in the accumulator.
printA :: Z80ASM
printA = rst 16

-- | Print a value by loading it into the accumulator and then calling "printA".
printVal :: Load A c => c -> Z80ASM
printVal c = ld A c >> printA

-- | Sets the cursor position by printing the special AT metacharacter.
setCursorPos :: (Load A x, Load A y) => (x, y) -> Z80ASM
setCursorPos (x, y) = do
  printVal AT
  printVal y
  printVal x
