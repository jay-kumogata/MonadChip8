-- ----------------------------------------------------------------- --
--                                                                   --
--  PPU.hs : CHIP-8 Emulator                                         --
--                                                                   --
--  2011/01/03  jay1905 ( based on tanakah's implementation,         --
--                cf. http://fxp.infoseek.ne.jp/haskell/cpu.hs )     --
--                                                                   --
-- ----------------------------------------------------------------- --

module PPU (
  PPU,
  initPPU,
  erasePPU,
  setPixelPPU,
  xorPixelPPU,
  getPixelPPU,

  memHEXFONT,
  wFontTop,

  memVRAM,
  toVRAM,

  nWidth,
  nHeight,
) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Array
import Control.Monad
import Text.Printf

-- ----------------------------------------------------------------- --
-- PPU Resources                                                     --
-- ----------------------------------------------------------------- --

data PPU = PPU {
  -- Registers
  memVRAM :: Array Word16 Word8
}

-- Constants
nWidth :: Int
nWidth = 64
nHeight :: Int
nHeight = 32
wFontTop :: Word16
wFontTop = 0x0F10

-- Hexadecimal Fonts
memHEXFONT :: Array Word16 Word8
memHEXFONT = listArray (0x0,0x4F)
             [ 0xF0, 0x90, 0x90, 0x90, 0xF0,  -- 0
               0x20, 0x60, 0x20, 0x20, 0x70,  -- 1
               0xF0, 0x10, 0xF0, 0x80, 0xF0,  -- 2
               0xF0, 0x10, 0xF0, 0x10, 0xF0,  -- 3
               0x90, 0x90, 0xF0, 0x10, 0x10,  -- 4
               0xF0, 0x80, 0xF0, 0x10, 0xF0,  -- 5
               0xF0, 0x80, 0xF0, 0x90, 0xF0,  -- 6
               0xF0, 0x10, 0x20, 0x40, 0x40,  -- 7
               0xF0, 0x90, 0xF0, 0x90, 0xF0,  -- 8
               0xF0, 0x90, 0xF0, 0x10, 0xF0,  -- 9
               0xF0, 0x90, 0xF0, 0x90, 0x90,  -- A
               0xE0, 0x90, 0xE0, 0x90, 0xE0,  -- B
               0xF0, 0x80, 0x80, 0x80, 0xF0,  -- C
               0xE0, 0x90, 0x90, 0x90, 0xE0,  -- D
               0xF0, 0x80, 0xF0, 0x80, 0xF0,  -- E
               0xF0, 0x80, 0xF0, 0x80, 0x80 ] -- F

-- ----------------------------------------------------------------- --
-- Initialze PPU                                                     --
-- ----------------------------------------------------------------- --

initPPU :: IO PPU
initPPU = do 
  return PPU {
  -- Registers
    memVRAM = listArray (0x000, 0x7FF) [0,0..]
  }

-- ----------------------------------------------------------------- --
-- Show state of PPU                                                 --
-- ----------------------------------------------------------------- --

instance Show PPU where
  show ppu = showScreen where
    showScreen = concatMap (showLine) [0..nHeight-1]
    showLine y = ( concatMap (showDot y) [0..nWidth-1] ) ++ "\n"
    showDot y x =
      case (memVRAM ppu)!(toVRAM x y) of
        0x00 -> "-"
        0x01 -> "*"

-- ----------------------------------------------------------------- --
-- Functions                                                         --
-- ----------------------------------------------------------------- --

toVRAM :: Int -> Int -> Word16
toVRAM x y = toEnum $ (y `mod` nHeight) * nWidth + (x `mod` nWidth)

-- Erase screen
erasePPU :: PPU -> IO PPU
erasePPU ppu = 
  eraseAPPU ppu
    [(x,y) | x <- [0..nWidth-1], y <- [0..nHeight-1]]
    0x00

eraseAPPU :: PPU -> [(Int,Int)] -> Word8 -> IO PPU
eraseAPPU ppu pts c =
  case pts of 
    [] -> return $ ppu
    ((x,y):ps) -> do ppu <- setPixelPPU ppu x y c
                     eraseAPPU ppu ps c

-- Set Pixel
setPixelPPU :: PPU -> Int -> Int -> Word8 -> IO PPU
setPixelPPU ppu x y c =
  return $ ppu { memVRAM = memVRAM ppu // [(toVRAM x y, c)] }

-- Xor Pixel
xorPixelPPU :: PPU -> Int -> Int -> Word8 -> IO PPU
xorPixelPPU ppu x y c = do
  d <- getPixelPPU ppu x y
  return $ ppu { memVRAM = memVRAM ppu // [(toVRAM x y, c `xor` d)] }

-- Get Pixel
getPixelPPU :: PPU -> Int -> Int -> IO Word8
getPixelPPU ppu x y =
  return $ (memVRAM ppu)!(toVRAM x y)
