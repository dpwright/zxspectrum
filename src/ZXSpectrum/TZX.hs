{-# LANGUAGE OverloadedStrings #-}

{-| Write binary data out to .tzx format.
  Based on the format specification here: http://www.worldofspectrum.org/TZXformat.html
-}

module ZXSpectrum.TZX
  ( Header (..)
  , CompressionType (..)
  , StartingPolarity (..)
  , SymDef (..)
  , PRLE (..)
  , Block (..)
  , Selection (..)
  , SignalLevel (..)
  , TextType (..)
  , Text (..)
  , Capabilities (..)
  , Hardware (..)
  , HWInfo (..)
  , header
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Monoid
import Data.Word

data Header           = Header { version :: (Word8, Word8) }
data CompressionType  = RLE | ZRLE
data StartingPolarity = Opposite | Same | Low | High
data SymDef           = SymDef StartingPolarity [Word16]
data PRLE             = PRLE Word8 Word16
data Selection        = Selection Word16 Word8 String
data SignalLevel      = SignalLow | SignalHigh

data TextType         = Title | Publisher | Author | Year | Language
                      | Type | Price | Loader | Origin | Comment
data Text             = Text TextType Word8 String

data Capabilities     = Runs | Uses | Doesn'tUse | Doesn'tRun
data HWInfo           = HWInfo Word8 Word8 Capabilities

data Block
  = DataStandardSpeed Word16 Word16 ByteString
  | DataTurboSpeed    Word16 Word16 Word16 Word16 Word16 Word16 Word8 Word16 Int ByteString
  | PureTone          Word16 Word16
  | PulseSequence     Word16 [Word16]
  | DataPure          Word16 Word16 Word8 Word16 Int ByteString
  | DirectRecording   Word16 Word16 Word8 Int ByteString
  | CSWRecording      Word32 Word16 Int CompressionType Word32 ByteString
  | DataGeneralized   Word32 Word16 Word32 Word8 Word8 Word32 Word8 Word8 [SymDef] [PRLE] [SymDef] ByteString
  | Pause             Word16
  | GroupStart        Word8 String
  | GroupEnd
  | Jump              Word16
  | LoopStart         Word16
  | LoopEnd
  | Call              Word16 [Word16]
  | Return
  | Select            Word16 Word8 [Selection]
  | StopTheTape48K    Word32
  | SetSignalLevel    Word32 SignalLevel
  | Description       Word8 String
  | Message           Word8 Word8 String
  | ArchiveInfo       Word16 Word8 [Text]
  | HardwareType      Word8 [HWInfo]
  | CustomInfo        String Word32 ByteString
  | Glue

header :: Header -> ByteString
header (Header (major, minor)) = "ZXTape!" <> BS.pack [0x1a, major, minor]

data Hardware
  = ZXSpectrum16k
  | ZXSpectrum48kPlus
  | ZXSpectrum48kISSUE1
  | ZXSpectrum128kPlus
  | ZXSpectrum128kPlus2
  | ZXSpectrum128kPlus2APlus3
  | TimexSinclairTC2048
  | TimexSinclairTS2068
  | Pentagon128
  | SamCoupe
  | DidaktikM
  | DidaktikGama
  | ZX80
  | ZX81
  | ZXSpectrum128kSpanishversion
  | ZXSpectrumArabicversion
  | MicrodigitalTK90X
  | MicrodigitalTK95
  | Byte
  | Elwro8003
  | ZSScorpion256
  | AmstradCPC464
  | AmstradCPC664
  | AmstradCPC6128
  | AmstradCPC464Plus
  | AmstradCPC6128Plus
  | JupiterACE
  | Enterprise
  | Commodore64
  | Commodore128
  | InvesSpectrumPlus
  | Profi
  | GrandRomMax
  | Kay1024
  | IceFelixHC91
  | IceFelixHC2000
  | AmaterskeRADIOMistrum
  | Quorum128
  | MicroARTATM
  | MicroARTATMTurbo2
  | Chrome
  | ZXBadaloc
  | TS1500
  | Lambda
  | TK65
  | ZX97
  | ZXMicrodrive
  | OpusDiscoveryStorage
  | MGTDisciple
  | MGTPlusD
  | RotronicsWafadrive
  | TRDOS
  | ByteDrive
  | Watsford
  | FIZ
  | Radofin
  | Didaktikdiskdrives
  | BSDOS
  | ZXSpectrumPlus3diskdrive
  | JLODiskInterface
  | TimexFDD3000
  | Zebradiskdrive
  | RamexMillenia
  | Larken
  | Kempstondiskinterface
  | Sandy
  | ZXSpectrumPlus3eharddisk
  | ZXATASP
  | DivIDE
  | ZXCF
  | SamRam
  | MultifaceONE
  | Multiface128k
  | MultifacePlus3
  | MultiPrint
  | MB02ROMRAMexpansion
  | SoftROM
  | RAM1k
  | RAM16k
  | RAM48k
  | Memoryin816kused
  | ClassicAYhardware
  | FullerBoxAYsoundhardware
  | CurrahmicroSpeech
  | SpecDrum
  | AYACBstereo
  | AYABCstereo
  | RAMMusicMachine
  | Covox
  | GeneralSound
  | IntecElectronicsDigitalInterfaceB8001
  | ZonXAY
  | QuickSilvaAY
  | JupiterACESound
  | Kempston
  | CursorProtekAGF
  | Sinclair2Left
  | Sinclair1Right
  | Fuller
  | AMXmouse
  | Kempstonmouse
  | Trickstick
  | ZXLightGun
  | ZebraGraphicsTablet
  | DefenderLightGun
  | ZXInterface1Serial
  | ZXSpectrum128k
  | KempstonS
  | KempstonE
  | ZXSpectrumPlus3
  | Tasman
  | DK'Tronics
  | Hilderbay
  | INESPrinterface
  | ZXLPrintInterface3
  | MultiPrintParallel
  | OpusDiscoveryParallel
  | Standard8255chipwithports316395
  | ZXPrinterAlphacom32
  | Genericprinter
  | EPSONcompatible
  | PrismVTX5000
  | TS2050orWestridge2050
  | RDDigitalTracer
  | DK'TronicsLightPen
  | BritishMicroGraphPad
  | RomanticRobotVideoface
  | ZXInterface1Network
  | KeypadforZXSpectrum128k
  | HarleySystemsADC82
  | BlackboardElectronics
  | OrmeElectronics
  | WRXHiRes
  | G007
  | Memotech
  | LambdaColourata
