{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Keyboard support for the Spectrum 48k.

module ZXSpectrum.Keyboard
  ( pattern KEYS_BNMsssp
  , pattern KEYS_HJKLret
  , pattern KEYS_YUIOP
  , pattern KEYS_67890
  , pattern KEYS_54321
  , pattern KEYS_TREWQ
  , pattern KEYS_GFDSA
  , pattern KEYS_VCXZcap
  , ktest
  ) where

import Z80
import Prelude hiding (and)

pattern KEYS_BNMsssp = 0x7ffe :: Location
pattern KEYS_HJKLret = 0xbffe :: Location
pattern KEYS_YUIOP   = 0xdffe :: Location
pattern KEYS_67890   = 0xeffe :: Location
pattern KEYS_54321   = 0xf7fe :: Location
pattern KEYS_TREWQ   = 0xfbfe :: Location
pattern KEYS_GFDSA   = 0xfdfe :: Location
pattern KEYS_VCXZcap = 0xfefe :: Location

-- | Mr. Jones' keyboard test routine.
-- Taken from this blog post: https://chuntey.wordpress.com/2012/12/19/how-to-write-zx-spectrum-games-chapter-2/
-- from which I quote:
-- | "To use his routine, load the accumulator with the number of the key you
-- wish to test, call ktest, then check the carry flag. If it’s set the key is not
-- being pressed, if there’s no carry then the key is being pressed."
ktest :: Z80ASM
ktest = do
  ld C A              -- key to test in c.
  and 7               -- mask bits d0-d2 for row.
  inc A               -- in range 1-8.
  ld B A              -- place in b.
  srl C               -- divide c by 8,
  srl C               -- to find position within row.
  srl C
  ld A 5              -- only 5 keys per row.
  sub C               -- subtract position.
  ld C A              -- put in c.
  ld A 254            -- high byte of port to read.
  withLabel $ \ktest0 -> do
    rrca              -- rotate into position.
    djnz ktest0       -- repeat until we've found relevant row.
    in_ A [254]       -- read port (a=high, 254=low).
  withLabel $ \ktest1 -> do
    rra               -- rotate bit out of result.
    dec C             -- loop counter.
    jp NZ ktest1      -- repeat until bit for position in carry.
    ret
