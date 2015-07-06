{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities to help handle screen attributes

module ZXSpectrum.Attributes
  ( Colour (..)
  , Paper (..)
  , Ink (..)
  , pattern BLACK
  , pattern BLUE
  , pattern RED
  , pattern MAGENTA
  , pattern GREEN
  , pattern CYAN
  , pattern YELLOW
  , pattern WHITE
  , colourCode
  , Flash (..)
  , flashCode
  , Bright (..)
  , brightCode
  , AttributeType (..)
  , pattern ATTR_DEFAULT
  , pattern ATTR_TEMP
  , attrAddress
  , setAttrs
  , setBorderColour
  ) where

import Z80
import ZXSpectrum.Rom48

import Data.Word
import Data.Bits

data Colour = Black | Blue | Red | Magenta | Green | Cyan | Yellow | White
              deriving (Eq, Show)

pattern BLACK      = 0x0 :: Word8
pattern BLUE       = 0x1 :: Word8
pattern RED        = 0x2 :: Word8
pattern MAGENTA    = 0x3 :: Word8
pattern GREEN      = 0x4 :: Word8
pattern CYAN       = 0x5 :: Word8
pattern YELLOW     = 0x6 :: Word8
pattern WHITE      = 0x7 :: Word8

colourCode :: Colour -> Word8
colourCode Black   = BLACK
colourCode Blue    = BLUE
colourCode Red     = RED
colourCode Magenta = MAGENTA
colourCode Green   = GREEN
colourCode Cyan    = CYAN
colourCode Yellow  = YELLOW
colourCode White   = WHITE

data Paper = Paper Colour deriving (Eq, Show)
data Ink   = Ink Colour   deriving (Eq, Show)

data Flash = Flash | NoFlash
flashCode :: Flash -> Word8
flashCode Flash   = 0x80
flashCode NoFlash = 0

data Bright = Bright | NotBright
brightCode :: Bright -> Word8
brightCode Bright    = 0x40
brightCode NotBright = 0

data AttributeType = AttrDefault | AttrTemp

pattern ATTR_DEFAULT = 0x5c8d :: Location
pattern ATTR_TEMP    = 0x5c8f :: Location

attrAddress :: AttributeType -> Word16
attrAddress AttrDefault = ATTR_DEFAULT
attrAddress AttrTemp    = ATTR_TEMP

setAttrs :: AttributeType
         -> Flash  -- ^ FLASH mode
         -> Bright -- ^ BRIGHT mode
         -> Paper  -- ^ Paper colour
         -> Ink    -- ^ Ink colour
         -> Z80ASM
setAttrs attr flash bright (Paper paper) (Ink ink) =
  ldVia A [attrAddress attr] $ f .|. b .|. p .|. i
  where f = flashCode flash
        b = brightCode bright
        p = colourCode paper `shiftL` 3
        i = colourCode ink

setBorderColour :: Colour -> Z80ASM
setBorderColour border = do
  ld A $ colourCode border
  call BORDERFAST
