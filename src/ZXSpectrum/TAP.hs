{-# LANGUAGE OverloadedStrings #-}

{-|
Write binary data out to .tap format.
The BASIC code for the loader is based on that from bin2tap,
downloaded from here: http://metalbrain.speccy.org/link-eng.htm.

.tap files support concatenation, so you can join them together using `mappend`

For example, assuming you are using this package in conjunction with the 'Z80'
assembler, the following is enough to output a .tap file containing an insulting
program:

@
encodeBlock :: ASMBlock -> Either String ByteString
encodeBlock (ASMBlock orgAddr asm) =
  mappend \<$\> loader "youresilly" orgAddr orgAddr
          \<*\> codeBlock "silly" orgAddr asm

main = either putStrLn (BS.writeFile "test.tap") . encodeBlock . org 0x6000 $ mdo
  ld a 2                 -- upper screen
  call 5633              -- open channel

  withLabel $ \loop -> do
    ld de string         -- address of string
    ld bc (eostr-string) -- length of string to print
    call 8252            -- print our string
    jp loop              -- repeat until screen is full

  string <- labelled $ db "You are a silly "
  eostr  <- label
  end
@

Example based on that listed here: https://chuntey.wordpress.com/2012/12/18/how-to-write-zx-spectrum-games-chapter-1/
-}

module ZXSpectrum.TAP
  ( loader
  , codeBlock
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Word
import Data.Bits
import Data.String

import Control.Monad.Except
import Data.Monoid

data BlockType = Program | NumberArray | CharacterArray | Code
data Flags     = Hdr | Data

data Header = Header
  { hdrType   :: BlockType
  , hdrName   :: String
  , hdrLength :: Word16
  , hdrParam1 :: Word16
  , hdrParam2 :: Word16
  }

encodeBlockType :: BlockType -> ByteString
encodeBlockType Program        = BS.pack [0]
encodeBlockType NumberArray    = BS.pack [1]
encodeBlockType CharacterArray = BS.pack [2]
encodeBlockType Code           = BS.pack [3]

encodeFlags :: Flags -> ByteString
encodeFlags Hdr  = BS.pack [0x00]
encodeFlags Data = BS.pack [0xff]

encodeHeader :: (IsString s, Monoid s, MonadError s m) => Header -> m ByteString
encodeHeader hdr = withCheckedName (hdrName hdr) . mconcat $
  [ encodeBlockType (hdrType hdr)
  , fullName
  , BS.pack $ encodeWord (hdrLength hdr)
  , BS.pack $ encodeWord (hdrParam1 hdr)
  , BS.pack $ encodeWord (hdrParam2 hdr)
  ]
  where fullName = encodeName $ hdrName hdr

withHeader :: (IsString s, Monoid s, MonadError s m)
           => Header -> ByteString -> m ByteString
withHeader hdr bs = (<> bs) . tapBlock Hdr <$> encodeHeader hdr

tapBlock :: Flags -> ByteString -> ByteString
tapBlock flags bs = blockLength <> bs'
  where appendParity x = BS.snoc x $ BS.foldr xor 0 x
        blockLength    = BS.pack . encodeWord . fromIntegral $ BS.length bs'
        bs'            = appendParity $ encodeFlags flags <> bs

{-| A simple loader, which sets us up for running machine code from an address in memory. -}
loader :: (IsString s, Monoid s, MonadError s m)
       => String       -- ^ Name of the loader program (max. 10 characters).
       -> Word16       -- ^ Where to load the machine code into in memory.
       -> Word16       -- ^ Address to begin execution (often matches the previous parameter).
       -> m ByteString -- ^ Raw loader code data, ready to be written to a file.
loader name datastart exeat = withHeader hdr $ tapBlock Data loaderCode
  where hdr         = Header Program name lineLength (fromIntegral lineNumber) lineLength
        lineLength  = fromIntegral $ BS.length loaderCode
        lineNumber  = 10
        loaderCode  = basic lineNumber . basicSeq $ map BS.pack
                    [ [clear, val] <> quotedNum (datastart-1)
                    , [load] <> quoted [] <> [screen]
                    , [poke, val] <> quotedNum 23739 <> [encodeChar ','] <> [val] <> quotedNum 111
                    , [load] <> quoted [] <> [code]
                    , [randomize, usr, val] <> quotedNum exeat ]

{-| Encode machine code into .tap format, ready for loading. -}
codeBlock :: (IsString s, Monoid s, MonadError s m)
          => String       -- ^ Name of the program (max. 10 characters).
          -> Word16       -- ^ Where to load the machine code into in memory.
          -> ByteString   -- ^ The raw machine code as output from an assembler
          -> m ByteString -- ^ The data in .tap form, ready to be written to a file.
codeBlock name datastart asm = withHeader hdr $ tapBlock Data asm
  where hdr = Header Code name len datastart 32768
        len = fromIntegral $ BS.length asm

withCheckedName :: (IsString s, Monoid s, MonadError s m) => String -> a -> m a
withCheckedName n x
  | length n > 10 = throwError $ "Filename too long! (max. 10 letters): " <> fromString n
  | otherwise     = return x

-- ENCODING HELPERS

encodeName :: String -> ByteString
encodeName = BS.pack . map encodeChar . take 10 . (++ repeat ' ')

encodeChar :: Char -> Word8
encodeChar = fromIntegral . fromEnum

encodeWord :: Word16 -> [Word8]
encodeWord w = [lo w, hi w]

encodeLineNumber :: Word16 -> [Word8]
encodeLineNumber w = [hi w, lo w]

encodeDecimal ::  Word16 -> [Word8]
encodeDecimal = map encodeChar . show

lo, hi :: Word16 -> Word8
lo w = fromIntegral $ w `mod` 256
hi w = fromIntegral $ w `div` 256

-- ZX BASIC HELPERS
-- These functions are here to make writing the BASIC code for the loader a little
-- easier.  Of course, some sort of fully-fledged DSL for writing BASIC code would
-- be even better, but for now these serve as a useful stop-gap
basic :: Word16 -> ByteString -> ByteString
basic ln c = lineNumber <> lineLength <> code'
  where lineLength = BS.pack . encodeWord . fromIntegral $ BS.length code'
        lineNumber = BS.pack . encodeLineNumber $ ln
        code'      = c <> BS.pack [cr]

clear, val, load, code, screen  :: Word8
poke, randomize, usr, quote, cr :: Word8
clear     = 0xfd
val       = 0xb0
load      = 0xef
code      = 0xaf
screen    = 0xaa
poke      = 0xf4
randomize = 0xf9
usr       = 0xc0
quote     = encodeChar '\"'
cr        = encodeChar '\r'

quoted :: [Word8] -> [Word8]
quoted s = [quote] <> s <> [quote]

quotedNum :: Word16 -> [Word8]
quotedNum n = quoted $ encodeDecimal n

basicSeq :: [ByteString] -> ByteString
basicSeq = BS.intercalate (BS.pack $ [encodeChar ':'])
