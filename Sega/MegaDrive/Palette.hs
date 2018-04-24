{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Sega.MegaDrive.Palette (
  ColorNibble(..)
, BGR(..)
, nibbleToByte
, readColorNibble
, readColor
, readPalette
) where

import Control.Applicative (liftA3)
import Data.Bits
import Data.Word (Word8, Word16)
import qualified Data.ByteString as BS

data ColorNibble
  = C_0
  | C_2
  | C_4
  | C_6
  | C_8
  | C_A
  | C_C
  | C_E
  deriving (Eq, Ord, Show)

nibbleToByte :: ColorNibble -> Word8
nibbleToByte C_0 =
  0x00
nibbleToByte C_2 =
  0x20
nibbleToByte C_4 =
  0x40
nibbleToByte C_6 =
  0x60
nibbleToByte C_8 =
  0x80
nibbleToByte C_A =
  0xA0
nibbleToByte C_C =
  0xC0
nibbleToByte C_E =
  0xE0

data BGR a
  = BGR a a a
  deriving (Eq, Ord, Show, Functor, Foldable)

readColorNibble :: Word8 -> Maybe ColorNibble
readColorNibble 0x0 =
  Just C_0
readColorNibble 0x2 =
  Just C_2
readColorNibble 0x4 =
  Just C_4
readColorNibble 0x6 =
  Just C_6
readColorNibble 0x8 =
  Just C_8
readColorNibble 0xA =
  Just C_A
readColorNibble 0xC =
  Just C_C
readColorNibble 0xE =
  Just C_E
readColorNibble _ =
  Nothing

readColor :: Word16 -> Maybe (BGR ColorNibble)
readColor w =
  if w .&. 0xF000 /= 0
  then Nothing
  else liftA3 BGR (readColorNibble b) (readColorNibble g) (readColorNibble r)
  where
    b =
      fromIntegral (w `shiftR` 8)
    g =
      fromIntegral ((w .&. 0xF0) `shiftR` 4)
    r =
      fromIntegral (w .&. 0xF)

word16s :: [Word8] -> [Word16]
word16s (a:b:xs) =
  ((fromIntegral a `shiftL` 8) .|. fromIntegral b) : word16s xs
word16s _ =
  []

readPalette :: BS.ByteString -> Maybe [BGR ColorNibble]
readPalette =
  traverse readColor . word16s . BS.unpack
