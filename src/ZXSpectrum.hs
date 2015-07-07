module ZXSpectrum
  ( defaultMain
  , module ZXSpectrum.Rom48
  , module ZXSpectrum.Sound
  , module ZXSpectrum.Keyboard
  , module ZXSpectrum.Attributes
  ) where

import Z80
import ZXSpectrum.TAP
import ZXSpectrum.Rom48
import ZXSpectrum.Sound
import ZXSpectrum.Keyboard
import ZXSpectrum.Attributes

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

encodeBlock :: String -> ByteString -> ASMBlock -> Either String ByteString
encodeBlock loaderName scr (ASMBlock orgAddr asm) = do
  loaderBlock  <- loader loaderName orgAddr orgAddr
  titleBlock   <- codeBlock "" 16384 scr
  contentBlock <- codeBlock "" orgAddr asm
  return $ mconcat [loaderBlock, titleBlock, contentBlock]

{-| Provides a simple main function to make writing spectrum programs easy. -}
defaultMain :: String   -- ^ The name to give the loader file (max. 10 characters)
            -> FilePath -- ^ Path to the .scr file for the loading screen
            -> ASMBlock -- ^ Machine code for the program
            -> IO ()
defaultMain loaderName loadingScreen asm = do
  scr <- BS.readFile loadingScreen
  either handleError (BS.writeFile "a.tap") $ encodeBlock loaderName scr asm
  where handleError = putStrLn . ("Error: " ++)
