{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-| Write binary data out to .tap format.
  For now just porting bin2tap, downloaded from here: http://metalbrain.speccy.org/link-eng.htm
-}

module ZXSpectrum.TAP
  ( encode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Word
import Data.Bits
import Data.String

import Control.Monad.Except
import Control.Monad.RWS

encode :: (IsString s, Monoid s, MonadError s m, MonadRWS ByteString ByteString Word8 m)
       => String -> Word16 -> Word16 -> m ()
encode name datastart exeat
  | length name > 10 = throwError $ "Filename too long! (max. 10 letters): " <> fromString name
  | otherwise = do
      let fullName = BS.pack . map decodeChar . take 10 $ name ++ repeat ' '
      tell $ BS.pack [0x13, 0, 0]
      put 0x31
      writeByte 0

      -- loader
      tell "loader    "
      writeByte 0x1e      -- line length
      writeByte 0
      writeByte 0x0a      -- 10
      writeByte 0
      writeByte 0x1e      -- line length
      writeByte 0
      writeByte 0x1b
      writeByte 0x20
      writeByte 0
      writeByte 0xff
      writeByte 0
      writeByte 0x0a
      writeByte 0x1a
      writeByte 0
      writeByte 0xfd      -- CLEAR
      writeByte 0xb0      -- VAL
      writeChar '\"'
      writeDecimal 4 (datastart-1)
      writeChar '\"'
      writeChar ':'
      writeByte 0xef      -- LOAD
      writeChar '\"'
      writeChar '\"'
      writeByte 0xaf      -- CODE
      writeChar ':'
      writeByte 0xf9      -- RANDOMIZE
      writeByte 0xc0      -- USR
      writeByte 0xb0      -- VAL
      writeChar '\"'
      writeDecimal 4 exeat
      writeChar '\"'
      writeByte 0x0d
      writeByte =<< get

      -- code header
      tell $ BS.pack [19, 0, 0]
      put 0
      writeByte 3 -- Filetype (code)
      writeByteString fullName

      -- data
      asm <- ask
      let len = fromIntegral $ BS.length asm
      writeWord len
      writeWord datastart -- load address
      writeWord 0         -- offset
      writeByte =<< get

      -- Now onto the data bit
      writeWord $ len + 2
      put 0
      writeByte 255
      writeByteString asm
      writeByte =<< get


writeDecimal :: (MonadRWS r ByteString Word8 m) => Word16 -> Word16 -> m ()
writeDecimal 0 = \n -> tell (BS.pack [fromIntegral $ (n `mod` 10) + 48])
writeDecimal e = writeTens (10^e) >=> writeDecimal (e-1)
  where writeTens x n = tell (BS.pack [fromIntegral c + 48]) >> return (n - c*x)
          where c = n `div` x

writeWord :: (MonadRWS r ByteString Word8 m) => Word16 -> m ()
writeWord w = writeByte lo >> writeByte hi
  where lo = fromIntegral $ w `mod` 256
        hi = fromIntegral $ w `div` 256

writeByte :: (MonadRWS r ByteString Word8 m) => Word8 -> m ()
writeByte b = tell (BS.pack [b]) >> modify (xor b)

writeChar :: (MonadRWS r ByteString Word8 m) => Char -> m ()
writeChar = writeByte . decodeChar

writeByteString :: (MonadRWS r ByteString Word8 m) => ByteString -> m ()
writeByteString = mapMBS_ writeByte

mapMBS_ :: (Monad m) => (Word8 -> m ()) -> ByteString -> m ()
mapMBS_ a = BS.foldr (\b m -> a b >> m) $ return ()

decodeChar :: Char -> Word8
decodeChar = fromIntegral . fromEnum
