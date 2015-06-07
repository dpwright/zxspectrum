module ZXSpectrum
  ( defaultMain
  ) where

import Z80
import ZXSpectrum.TAP

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

encodeBlock :: String -> String -> ASMBlock -> Either String ByteString
encodeBlock loaderName programName (ASMBlock orgAddr asm) =
  mappend <$> loader loaderName orgAddr orgAddr
          <*> codeBlock programName orgAddr asm

{-| Provides a simple main function to make writing spectrum programs easy. -}
defaultMain :: String   -- ^ The name to give the loader file (max. 10 characters)
            -> String   -- ^ The name to give the program file (max. 10 characters)
            -> ASMBlock -- ^ Machine code for the program
            -> IO ()
defaultMain loaderName programName =
  either handleError (BS.writeFile "a.tap") . encodeBlock loaderName programName
  where handleError = putStrLn . ("Error: " ++)
