{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Sega.MegaDrive.Palette (
  ColorNibble(..)
, Color(..)
, Palette(..)
, nibbleToByte
, readColorNibble
, readColor
, readPalette
) where

import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Word
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

data Color a
  = Color a a a
  deriving (Eq, Ord, Show, Functor, Foldable)

data Palette a
  = Palette a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
            a
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

readColor :: Word16 -> Maybe (Color ColorNibble)
readColor w =
  if w .&. 0xF000 /= 0
  then Nothing
  else liftA3 Color (readColorNibble b) (readColorNibble g) (readColorNibble r)
  where
    b =
      fromIntegral (w `shiftR` 8)
    g =
      fromIntegral ((w .&. 0xF0) `shiftR` 4)
    r =
      fromIntegral (w .&. 0xF)

word16be :: StateT BS.ByteString Maybe Word16
word16be = do
  bs <- get
  (a, bs') <- lift (BS.uncons bs)
  (b, bs'') <- lift (BS.uncons bs')
  put bs''
  return ((fromIntegral a `shiftL` 8) .|. fromIntegral b)

readPalette :: BS.ByteString -> Maybe (Palette (Color ColorNibble))
readPalette =
  evalStateT p
  where
    c = do
      word16be >>= lift . readColor
    p =
      Palette <$> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
              <*> c
