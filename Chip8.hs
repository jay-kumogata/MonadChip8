-- ----------------------------------------------------------------- --
--                                                                   --
--  System.hs : CHIP-8 Emulator                                        --
--                                                                   --
--  2010/12/17  jay1905 ( based on tanakah's implementation,         --
--                cf. http://fxp.infoseek.ne.jp/haskell/cpu.hs )     --
--                                                                   --
-- ----------------------------------------------------------------- --

module Chip8 (
  initSystem,
  runSystem,
) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Array
import Control.Monad
import Text.Printf
import Data.ByteString.Lazy

import CPU
import PPU
import IOU
  
-- ----------------------------------------------------------------- --
--   Initialize Resources                                            --
-- ----------------------------------------------------------------- --
initSystem :: String -> IO CPU
initSystem fname = do
  -- Initialize CPU
  a <- initCPU

  -- Load ROM image
  s <- Data.ByteString.Lazy.readFile fname
  a <- loadRom a 0x200 $ unpack s

  -- Successful
  return a

runSystem :: CPU -> IO (CPU,Int)
runSystem cpu = do
  -- Main loop
  -- while ( 1 ) :
  --(cpu,d) <- execCPU10 cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu

  -- telmac 1800 runs at 3.2MHz and 1 instrution takes 16 clocks in average.
  -- 1 chip8 operation takes 500 instructions in average.
  -- so chip8 runs at 400Hz ( = 3.2MHz / (16 * 500) ).
  -- this function will be called per 1/60 second.
  -- so 6.67 chip8 operations will be executed per 1/60 second.

  --  VSYNC occurs per 1/60 second
  iou <- return $ ioIOU cpu

  -- Delay timer count down
  iou <- return $
    if (ioDelay iou) > 0
    then iou { ioDelay = (ioDelay iou) - 1 }
    else iou

  -- Sound timer count down
  iou <- return $
    if (ioSound iou) > 0
    then iou { ioSound = (ioSound iou) - 1 }
    else iou

  cpu <- return $ cpu { ioIOU = iou }

  -- Screen Shot
  -- showPPU cpu

  return (cpu,d)

-- ----------------------------------------------------------------- --
-- Load ROM image                                                    --
-- ----------------------------------------------------------------- --

loadRom :: CPU -> Word16 -> [Word8] -> IO CPU
loadRom cpu adr str = case str of
  []     -> return cpu
  (c:cs) -> do cpu <- memWrite cpu adr c
               loadRom cpu (adr+1) cs

-- End of Chip8.hs

