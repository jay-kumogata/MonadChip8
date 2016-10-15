-- ----------------------------------------------------------------- --
--                                                                   --
--  MonadChip8.hs : CHIP-8 Emulator                                  --
--                                                                   --
--  2016/10/03  Jay Kumogata(twitter: jay1905)                       --
--                                                                   --
-- ----------------------------------------------------------------- --

import Data.Bits
import Data.Int
import Data.Word
import Data.Array
import Control.Monad
import Text.Printf
import Data.ByteString.Lazy
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import System.Environment(getArgs)
import System.Exit

import Chip8
import CPU
import PPU
import IOU

toFloat = fromIntegral
inv = complement

mag :: Int
mag = 8

-- ----------------------------------------------------------------- --
--   Main Routine                                                    --
-- ----------------------------------------------------------------- --

window :: Display
window = InWindow "MonadChip8 v0.1J" (nWidth*mag,nHeight*mag) (0,0)

background :: Color
background = black

-- Start Chip8's System
main :: IO ()
main = do
  printf "MonadChip8 v0.1J: A CHIP8 emulator in Haskell\n"
  printf "Copyright (c) 2016 Jay Kumogata "
  printf "All Rights Reserved.\n"

  -- Initialize Chip8's System
  args <- getArgs

  if Prelude.length args /= 1
    then die "\nUsage: MonadChip8 <ROM file name>"
    else printf ""
    
  fname <- return $ Prelude.head args

  -- Create Chip8's System
  cpu <- initSystem fname

  -- Start Chip8's System
  G.playIO window background 60
    cpu   -- initial system state
    blit  -- render a screen 
    input -- update system state when key event occurs
    run   -- update system state 

-- ------------------------------------------------------------ --
--   Window System                                              --
-- ------------------------------------------------------------ --

-- Blit a screen
blit :: CPU -> IO Picture
blit cpu = do
  ppu <- return $ ioPPU cpu
  drawing <- return $ Prelude.map
                      (pset ppu)
                      [(x,y) | x<-[0..nWidth-1], y <-[0..nHeight-1] ]
  return $ pictures drawing

-- Point set    
pset :: PPU -> (Int,Int) -> Picture
pset ppu (x,y) = 
  case (memVRAM ppu)!(toVRAM x y) of
    -- black pixel
    0x00      -> translate
                 (toFloat ((x*mag)-(nWidth*mag `div` 2)))
                 (toFloat (((nHeight-y)*mag)-(nHeight*mag `div` 2))) $
                 color black $
                 rectangleSolid (toFloat mag) (toFloat mag)
    -- white pixel
    otherwise -> translate
                 (toFloat ((x*mag)-(nWidth*mag `div` 2)))
                 (toFloat (((nHeight-y)*mag)-(nHeight*mag `div` 2))) $
                 color white $
                 rectangleSolid (toFloat mag-1) (toFloat mag-1)

-- Key layout
-- original -> qwerty
-- 123C     -> 4567
-- 456D     -> rtyu
-- 789E     -> fghj
-- A0BF     -> vbnm

-- Key Event 
input :: G.Event -> CPU -> IO CPU

-- Key Down
input (G.EventKey (G.Char 'b') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 0) } }
input (G.EventKey (G.Char '4') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 1) } }
input (G.EventKey (G.Char '5') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 2) } }
input (G.EventKey (G.Char '6') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 3) } }
input (G.EventKey (G.Char 'r') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 4) } }
input (G.EventKey (G.Char 't') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 5) } }
input (G.EventKey (G.Char 'y') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 6) } }
input (G.EventKey (G.Char 'f') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 7) } }
input (G.EventKey (G.Char 'g') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 8) } }
input (G.EventKey (G.Char 'h') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 9) } }
input (G.EventKey (G.Char 'v') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 10) } }
input (G.EventKey (G.Char 'n') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 11) } }
input (G.EventKey (G.Char '7') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 12) } }
input (G.EventKey (G.Char 'u') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 13) } }
input (G.EventKey (G.Char 'j') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 14) } }
input (G.EventKey (G.Char 'm') G.Down _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .|. (shiftL 1 15) } }

-- KEYUP
input (G.EventKey (G.Char 'b') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 0) } }
input (G.EventKey (G.Char '4') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 1) } }
input (G.EventKey (G.Char '5') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 2) } }
input (G.EventKey (G.Char '6') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 3) } }
input (G.EventKey (G.Char 'r') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 4) } }
input (G.EventKey (G.Char 't') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 5) } }
input (G.EventKey (G.Char 'y') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 6) } }
input (G.EventKey (G.Char 'f') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 7) } }
input (G.EventKey (G.Char 'g') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 8) } }
input (G.EventKey (G.Char 'h') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 9) } }
input (G.EventKey (G.Char 'v') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 10) } }
input (G.EventKey (G.Char 'n') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 11) } }
input (G.EventKey (G.Char '7') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 12) } }
input (G.EventKey (G.Char 'u') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 13) } }
input (G.EventKey (G.Char 'j') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 14) } }
input (G.EventKey (G.Char 'm') G.Up _ (_,_)) cpu = do
  iou <- return $ ioIOU cpu
  return $ cpu { ioIOU = iou { ioKey = (ioKey iou) .&. inv (shiftL 1 15) } }
input _ cpu = do return cpu

-- Run system 
run :: Float -> CPU -> IO CPU
run _ cpu = do
  (cpu,d) <- runSystem cpu
  return cpu

-- End of MonadChip8.hs

