{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Sound support for the Spectrum 48k.
-- Largely based on this post: https://chuntey.wordpress.com/2013/02/28/how-to-write-zx-spectrum-games-chapter-3/.

-- | Most of the stuff in here uses (at least) the HL, BC, DE, and IX registers.  If you are
-- making use of any of these, save them to the stack before calling these routines!

module ZXSpectrum.Sound
  ( Note (..)
  , note
  , pitchCode
  , play
  , playSeq
  , pitchBend
  ) where

import Z80
import ZXSpectrum.Rom48

import Data.Word

data Note = C_  | CS_ | D_  | DS_ | E_  | F_
          | FS_ | G_  | GS_ | A_  | AS_ | B_
          deriving (Eq, Show)

raiseOctave :: Float -> Word8 -> Float
raiseOctave freq octave = iterate (*2) freq !! fromIntegral octave

note :: Note  -- ^ The musical note we're interested in
     -> Word8 -- ^ The octave (in scientific pitch notation)
     -> Float -- ^ The frequency of that note at that octave
note C_  oct = 16.352 `raiseOctave` oct
note CS_ oct = 17.324 `raiseOctave` oct
note D_  oct = 18.354 `raiseOctave` oct
note DS_ oct = 19.445 `raiseOctave` oct
note E_  oct = 20.602 `raiseOctave` oct
note F_  oct = 21.827 `raiseOctave` oct
note FS_ oct = 23.125 `raiseOctave` oct
note G_  oct = 24.500 `raiseOctave` oct
note GS_ oct = 25.957 `raiseOctave` oct
note A_  oct = 27.500 `raiseOctave` oct
note AS_ oct = 29.135 `raiseOctave` oct
note B_  oct = 30.868 `raiseOctave` oct

pitchCode :: Float -> Word16
pitchCode freq = round $ 437500 / freq - 30.125

play :: Float -- ^ Frequency
     -> Float -- ^ Duration
     -> Z80ASM
play f duration = do
  ld HL $ pitchCode f
  ld DE . round $ f * duration
  call BEEPER

playSeq :: [(Float, Float)] -- ^ (Frequency, Duration)
        -> Z80ASM
playSeq = sequence_ . map (uncurry play)

pitchBend :: Float -> Word8 -> Z80ASM
pitchBend startingFreq iterations = do
  ld HL (pitchCode startingFreq) -- starting pitch.
  decLoopB iterations $ do
    push BC
    push HL           -- store pitch.
    ld DE 1           -- very short duration.
    call BEEPER       -- ROM beeper routine.
    pop HL            -- restore pitch.
    dec HL            -- pitch going up.
    pop BC
