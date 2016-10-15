-- ----------------------------------------------------------------- --
--                                                                   --
--  CPU.hs : CHIP-8 Emulator                                         --
--                                                                   --
--  2010/12/17  jay1905 ( based on tanakah's implementation,         --
--                cf. http://fxp.infoseek.ne.jp/haskell/cpu.hs )     --
--                                                                   --
-- ----------------------------------------------------------------- --

module CPU (
  CPU,
  initCPU,
  execCPU,
  execCPU10,
  --execCPU100,
  memRead,
  memWrite,
  --showPPU,
  ioPPU,
  ioIOU,
) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Array
import Control.Monad
import Text.Printf
import System.IO
import System.Random
import Data.ByteString.Internal

import PPU
import IOU

-- ----------------------------------------------------------------- --
-- CPU Resources                                                     --
-- ----------------------------------------------------------------- --

data CPU = CPU {
  -- CPU Registers
    vReg :: Array Word8 Word8
  , pcReg, iReg, instReg :: Word16
  -- CPU Stacks
  , stackReg :: Array Word8 Word16
  , spReg :: Word8 
  -- Memory Module
  , memRAM :: Array Word16 Word8
  -- PPU I/O
  , ioPPU :: PPU
  -- IOU I/O
  , ioIOU :: IOU
  }

-- ----------------------------------------------------------------- --
-- Initialze CPU                                                     --
-- ----------------------------------------------------------------- --

initCPU :: IO CPU
initCPU = do
  ppu <- initPPU
  iou <- initIOU
  return CPU { 
  -- CPU Registers
    vReg = listArray (0x0,0xF) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  , pcReg = 0x0200, iReg = 0x0000, instReg = 0x0000
  -- CPU Stacks
  , stackReg = listArray (0x0,0xF) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  , spReg = 0xF
  -- Memory Module
  , memRAM = listArray (0x000,0x7FF) [0,0..]
  -- PPU I/O
  , ioPPU = ppu
  -- IOU I/O
  , ioIOU = iou
  }

--showPPU :: CPU -> IO PPU
--showPPU cpu = do 
--  return $ ioPPU cpu

-- ----------------------------------------------------------------- --
-- Show state of CPU                                                 --
-- ----------------------------------------------------------------- --

instance Show CPU where
  show cpu = unlines $ [regv8,reg16,regv16,reg8 ] where
    reg8 = concatMap (showReg 2) [("SP", spReg )] 
    regv8 = concatMap (showvReg 2) [("V0", vReg, 0x0 ), 
                                    ("V1", vReg, 0x1 ), 
                                    ("V2", vReg, 0x2 ),
                                    ("V3", vReg, 0x3 ),
                                    ("V4", vReg, 0x4 ),
                                    ("V5", vReg, 0x5 ),
                                    ("V6", vReg, 0x6 ),
                                    ("V7", vReg, 0x7 ),
                                    ("V8", vReg, 0x8 ),
                                    ("V9", vReg, 0x9 ),
                                    ("VA", vReg, 0xA ),
                                    ("VB", vReg, 0xB ),
                                    ("VC", vReg, 0xC ),
                                    ("VD", vReg, 0xD ),
                                    ("VE", vReg, 0xE ),
                                    ("VF", vReg, 0xF )] 
    reg16 = concatMap (showReg 4) [("PC", pcReg),
                                   ("I", iReg),
                                   ("INST", instReg)]
    regv16 = concatMap (showvReg 4) [("S0", stackReg, 0x0 ), 
                                     ("S1", stackReg, 0x1 ), 
                                     ("S2", stackReg, 0x2 ),
                                     ("S3", stackReg, 0x3 ),
                                     ("S4", stackReg, 0x4 ),
                                     ("S5", stackReg, 0x5 ),
                                     ("S6", stackReg, 0x6 ),
                                     ("S7", stackReg, 0x7 ),
                                     ("S8", stackReg, 0x8 ),
                                     ("S9", stackReg, 0x9 ),
                                     ("SA", stackReg, 0xA ),
                                     ("SB", stackReg, 0xB ),
                                     ("SC", stackReg, 0xC ),
                                     ("SD", stackReg, 0xD ),
                                     ("SE", stackReg, 0xE ),
                                     ("SF", stackReg, 0xF )] 

    showReg n (s,f) = s ++ " : 0x" ++ toHex n (toEnum $ fromEnum $ f cpu) ++ " "
    showvReg n (s,f,i) = s ++ " : 0x" ++ toHex n (toEnum $ fromEnum $ f cpu ! i ) ++ " "

toHex :: Int -> Int -> String
toHex n = reverse . map ((tbl!!).(`mod`16)) . take n . iterate (`div`16)
  where tbl="0123456789ABCDEF"

-- ----------------------------------------------------------------- --
-- Auxiliary                                                         --
-- ----------------------------------------------------------------- --

to8_16 = toEnum . fromEnum
to16_8 = toEnum . fromEnum

read8 :: Word16 -> CPU -> IO Word8
read8 adr cpu = memRead cpu adr

read16 :: Word16 -> CPU -> IO Word16
read16 adr cpu = do
  h <- memRead cpu adr
  l <- memRead cpu (adr+1)
  return $ to8_16 l .|. (to8_16 h `shiftL` 8)

write8 :: Word16 -> Word8 -> CPU -> IO CPU
write8 adr dat cpu = memWrite cpu adr dat

write16 :: Word16 -> Word16 -> CPU -> IO CPU
write16 adr dat cpu = do
  cpu <- memWrite cpu (adr+1) $ to16_8 $ dat .&. 0x00FF
  cpu <- memWrite cpu  adr    $ to16_8 $ shiftR dat 8
  return cpu

push16 :: Word16 -> CPU -> IO CPU
push16 dat cpu = do
  cpu <- return $ cpu { stackReg = stackReg cpu // [(spReg cpu .&. 0xF, dat)] }
  cpu <- return $ incSP 0xFF cpu
  return cpu

pop16 :: CPU -> IO (CPU,Word16)
pop16 cpu = do
  cpu <- return $ incSP 1 cpu
  dat <- return $ (stackReg cpu)!(spReg cpu .&. 0xF) 
  return (cpu, dat)

incPC :: Word16 -> CPU -> CPU
incPC d cpu = cpu { pcReg = pcReg cpu + d }

incSP :: Word8 -> CPU -> CPU
incSP d cpu = cpu { spReg = spReg cpu + d }

readOpr16 :: CPU -> IO (CPU,Word16)
readOpr16 cpu = do
  d <- read16 (pcReg cpu) cpu
  return (incPC 2 cpu,d)

-- ----------------------------------------------------------------- --
-- Macros                                                            --
-- ----------------------------------------------------------------- --
mVX cpu   = (vReg cpu)!(mX cpu)
mVY cpu   = (vReg cpu)!(mY cpu)
mVF cpu   = (vReg cpu)!0xF
mV0 cpu   = (vReg cpu)!0x0
mVN cpu n = (vReg cpu)!n

mNNN cpu = (instReg cpu) .&. 0x0FFF
mKK cpu  = to16_8 $ (instReg cpu) .&. 0x00FF
mI cpu   = to16_8 $ shiftR ((instReg cpu) .&. 0xF000) 12
mX cpu   = to16_8 $ shiftR ((instReg cpu) .&. 0x0F00) 8
mY cpu   = to16_8 $ shiftR ((instReg cpu) .&. 0x00F0) 4
mT cpu   = to16_8 $ shiftR ((instReg cpu) .&. 0x000F) 0
mTT cpu  = to16_8 $ shiftR ((instReg cpu) .&. 0x00FF) 0

sVX cpu dat   = cpu { vReg = vReg cpu // [(mX cpu, dat)] }
sVY cpu dat   = cpu { vReg = vReg cpu // [(mY cpu, dat)] }
sVF cpu dat   = cpu { vReg = vReg cpu // [(0xF, dat)] }
sV0 cpu dat   = cpu { vReg = vReg cpu // [(0x0, dat)] }
sVN cpu n dat = cpu { vReg = vReg cpu // [(n, dat)] }

sIReg cpu dat = cpu { iReg = dat }

-- ----------------------------------------------------------------- --
-- Instructions                                                      --
-- ----------------------------------------------------------------- --

type Operation = CPU -> IO (CPU,Int)

opTbl = empty // ops where
  empty = listArray (0x0,0xf) [op0000]
  ops :: [(Word8,Operation)]
  ops = [(0x0,op0000),(0x1,op1000),(0x2,op2000),(0x3,op3000),
         (0x4,op4000),(0x5,op5000),(0x6,op6000),(0x7,op7000),
         (0x8,op8000),(0x9,op9000),(0xA,opA000),(0xB,opB000),
         (0xC,opC000),(0xD,opD000),(0xE,opE000),(0xF,opF000)]

op0000 cpu =
  case mTT cpu of 
    -- 0x00E0 : Erase the Screen (dummy) [/]
    0xE0 -> do
      -- printf "Erase the Screen\n"
      ppu <- erasePPU $ ioPPU cpu
      cpu <- return $ cpu { ioPPU = ppu }
      return (cpu,0)

    -- 0x00EE : Return from a CHIP-8 subroutine  [/]
    0xEE -> do
      (cpu,pc) <- pop16 cpu
      cpu <- return $ cpu { pcReg = pc }
      return (cpu,0)

-- 0x1NNN : Jump to NNN  [/]
op1000 cpu = do
  cpu <- return $ cpu { pcReg = mNNN cpu }
  return (cpu,1)

-- 0x2NNN : Call CHIP-8 sub-routine at NNN [/]
--          ( 16 successive calls max)
op2000 cpu = do
  cpu <- push16 (pcReg cpu) cpu
  cpu <- return $ cpu { pcReg = mNNN cpu }
  return (cpu,2)

-- 0x3XKK : Skip next instruction if VX == KK  [/]
op3000 cpu = do 
  cpu <- return $ if (mVX cpu == mKK cpu) 
                  then incPC 2 cpu else cpu
  return (cpu,3)

-- 0x4XKK : Skip next instruction if VX != KK
op4000 cpu = do
  cpu <- return $ if (mVX cpu /= mKK cpu)
                  then incPC 2 cpu else cpu
  return (cpu,4)

-- 0x5XY0 : Skip next instruction if VX == VY
op5000 cpu = do
  cpu <- return $ if (mVX cpu == mVY cpu)
                  then incPC 2 cpu else cpu
  return (cpu,5)

-- 0x6XKK : VX = KK
op6000 cpu = do
  cpu <- return $ sVX cpu $ mKK cpu
  return (cpu,6)

-- 0x7XKK : VX = VX + KK
op7000 cpu = do
  cpu <- return $ sVX cpu $ mVX cpu + mKK cpu
  return (cpu,7)

op8000 cpu = do
  cpu <- return $
    case mT cpu of 
      -- 0x8XY0 : VX = VY
      0x0 -> sVX cpu $ mVY cpu

      -- 0x8XY1 : VX = VX OR VY
      0x1 -> sVX cpu $ mVX cpu .|. mVY cpu

      -- 0x8XY2 : VX = VX AND VY
      0x2 -> sVX cpu $ mVX cpu .&. mVY cpu

      -- 0x8XY3 : VX = VX XOR VY
      0x3 -> sVX cpu $ mVX cpu `xor` mVY cpu

      -- 0x8XY4 : VX = VX + VY, VF = Carry
      0x4 -> sVX cpu2 $ mVX cpu2 + mVY cpu2
               where cpu2 = sVF cpu $ 
                              if (to8_16 (mVX cpu) + to8_16 (mVY cpu) > 0x00FF)
                                then 0x01 else 0x00

      -- 0x8XY5 : VX = VX - VY, VF = Not Borrow
      0x5 -> sVX cpu2 $ mVX cpu2 - mVY cpu2
               where cpu2 = sVF cpu $
                              if (mVX cpu < mVY cpu) then 0x00 else 0x01

      -- 0x8XY6 : VX = VX SHR 1 ( VX = VX / 2 ), VF = Carry
      0x6 -> sVX cpu2 $ shiftR (mVX cpu2) 1
               where cpu2 = sVF cpu $ (mVX cpu) .&. 0x01

      -- 0x8XY7 : VX = VY - VX, VF = Not Borrow
      0x7 -> sVX cpu2 $ mVY cpu2 - mVX cpu2
               where cpu2 = sVF cpu $ 
                              if (mVY cpu < mVX cpu) then 0x00 else 0x01

      -- 0x8XYE : VX = VX SHL 1 ( VX = VX * 2 ), VF = Carry
      0xE -> sVX cpu2 $ shiftL (mVX cpu2) 1
               where cpu2 = sVF cpu $ shiftR (mVX cpu .&. 0x80) 7

      -- Undefined
      otherwise -> cpu
  return (cpu,8)

-- 0x9XY0 : Skip next instruction if VX != VY
op9000 cpu = do
  cpu <- return $ if (mVX cpu /= mVY cpu)
                  then incPC 2 cpu else cpu
  return (cpu,9)

-- 0xANNN : I = NNN
opA000 cpu = do
  cpu <- return cpu { iReg = mNNN cpu }
  return (cpu,10)

-- 0xBNNN : Jump to NNN + V0
opB000 cpu = do
  cpu <- return cpu { pcReg = mNNN cpu + to8_16 (mV0 cpu) }
  return (cpu,11)

-- 0xCXKK : VX = Random number AND KK
opC000 cpu = do
  -- specification
  -- rnd <- randomRIO (0, 0xff) :: IO Word8

  -- for VBRIX
  rnd <- randomRIO (0, 0x1f) :: IO Word8

  cpu <- return $ sVX cpu $ rnd .&. (mKK cpu)

  -- printf "cxkk 0x%x 0x%x 0x%x 0x%x\n" (fromEnum $ mX cpu) (fromEnum $ mKK cpu) (fromEnum $ rnd) (fromEnum $ mVX cpu)
  return (cpu,12)

-- 0xDXYN : Draws a sprite (VX,VY) starting at M(I).
--          VF = collision.
opD000 cpu = do
  -- Draw sprite
  vx <- return $ fromEnum $ mVX cpu
  vy <- return $ fromEnum $ mVY cpu
  n <- return $ fromEnum $ mT cpu
  i <- return $ iReg cpu
  cpu <- drawSprite cpu vx vy n i

--  printf "Draws a sprite(0x%x,0x%x) starting at M(0x%x) hit(0x%x)\n" 
--         (fromEnum $ mVX cpu) (fromEnum $ mVY cpu) (fromEnum $ iReg cpu)
--         (fromEnum $ mVF cpu)   

  return (cpu,13)

-- Draw sprite
drawSprite :: CPU -> Int -> Int -> Int -> Word16 -> IO CPU
drawSprite cpu vx vy n i = do
  -- Clear Collision Detect Flag
  cpu <- return $ sVF cpu 0x00

  drawSpriteA cpu
    [(x,y) | x <- [0..7], y <- [0..n-1]]
    vx vy i

-- Draw sprite auxiliary
drawSpriteA :: CPU -> [(Int,Int)] -> Int -> Int -> Word16 -> IO CPU
drawSpriteA cpu pts vx vy i =
  case pts of
    [] -> return $ cpu
    ((x,y):ps) -> do
      ppu <- return $ ioPPU cpu
      byD <- read8 (i + toEnum y) cpu
      byS <- return $ byD .&. (shiftR 0x80 x)

      -- Set Collision Detect Flag
      cpu <- case byS of
        0x00      -> return $ cpu                 -- do nothing
        otherwise -> do
          p <- getPixelPPU ppu (vx+x) (vy+y)
          case p of
            0x00      -> return $ cpu             -- do nothing
            otherwise -> return $ sVF cpu 0x01    -- set collision detect flag

      -- Xor Pixel
      ppu <- case byS of
        0x00      -> return $ ppu
        otherwise -> xorPixelPPU ppu (vx+x) (vy+y) 0x01

      -- Update CPU info and process rest of points
      cpu <- return $ cpu { ioPPU = ppu }
      drawSpriteA cpu ps vx vy i

opE000 :: CPU -> IO (CPU,Int)
opE000 cpu = do
  iou <- return $ ioIOU cpu
  key <- return $ ioKey iou
  vx <- return $ fromEnum $ mVX cpu
--  printf "op%x will be called. : 0x%x 0x%x\n"
--    (fromEnum (instReg cpu)) (fromEnum key) vx

  cpu <-
    case mTT cpu of
      -- 0xEX9E : Skip next instruction if key VX pressed
      0x9E -> return $
        if ((key .&. (shiftL 1 vx)) /= 0)
        then incPC 2 cpu else cpu

      -- 0xEXA1 : Skip next instruction if key VX not pressed
      0xA1 -> return $
        if ((key .&. (shiftL 1 vx)) == 0)
        then incPC 2 cpu else cpu

  return (cpu,14)

opF000 cpu = do
  -- printf "opF0xx will be called. : 0x%x \n" (mTT cpu)
  iou <- return $ ioIOU cpu
  cpu <- 
    case mTT cpu of
      -- 0xFX07 : VX = Delay timer
      0x07 -> return $ sVX cpu $ ioDelay iou

      -- 0xFX0A : Waits a keypress and stores it in VX
      0x0A -> return $ cpu

      -- 0xFX15 : Delay timer = VX
      0x15 -> return $ cpu { ioIOU = iou { ioDelay = (mVX cpu) } }

      -- 0xFX18 : Sound timer = VX
      0x18 -> return $ cpu { ioIOU = iou { ioSound = (mVX cpu) } }

      -- 0xFX1E : I = I + VX
      0x1E -> return $ cpu { iReg = (iReg cpu) + to8_16 (mVX cpu) }

      -- 0xFX29 : I points to the 4 x 5 font sprite of hex
      --          char in VX
      0x29 -> return $ cpu { iReg = wFontTop + to8_16 (mVX cpu) * 5 }

      -- 0xFX33 : Store BCD representation of VX in M(I)..M(I+2)
      0x33 -> opFx33 cpu

      -- 0xFX55 : Save V0..VX in memory starting at M(I)
      0x55 -> opFx55 cpu [ x | x <- [0..(mX cpu)]]

      --  0xFX65 : Load V0..VX from memory starting at M(I)
      0x65 -> opFx65 cpu [ x | x <- [0..(mX cpu)]]

      -- Undefined
      otherwise -> return $ cpu

  return (cpu,15)

-- 0xFX33 : Store BCD representation of VX in M(I)..M(I+2)
opFx33 :: CPU -> IO CPU
opFx33 cpu =  do
  cpu <- write8  (iReg cpu)     ((mVX cpu) `div` 100) cpu
  cpu <- write8 ((iReg cpu)+1) (((mVX cpu) `mod` 100) `div` 10) cpu
  cpu <- write8 ((iReg cpu)+2)  ((mVX cpu) `mod` 10) cpu
  return cpu

-- 0xFX55 : Save V0..VX in memory starting at M(I)
opFx55 :: CPU -> [Word8] -> IO CPU
opFx55 cpu ids =
  case ids of
    [] -> return $ cpu
    (x:xs) -> do
      cpu <- write8 ((iReg cpu) + to8_16(x)) (mVN cpu x) cpu
      opFx55 cpu xs

--  0xFX65 : Load V0..VX from memory starting at M(I)
opFx65 :: CPU -> [Word8] -> IO CPU
opFx65 cpu ids =
  case ids of
    [] -> return $ cpu
    (x:xs) -> do
      dat <- read8 ((iReg cpu) + to8_16(x)) cpu
      cpu <- return $ sVN cpu x dat
      opFx65 cpu xs
       
execCPU :: CPU -> IO (CPU,Int)
execCPU cpu = do
  (cpu,d) <- readOpr16 cpu
  cpu <- return $ cpu { instReg = d }
  (opTbl!(mI cpu)) cpu

execCPU10 :: CPU -> IO (CPU,Int)
execCPU10 cpu = do
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu  
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu
  (cpu,d) <- execCPU cpu  
  return (cpu,d)

--execCPU100 :: CPU -> IO (CPU,Int)
--execCPU100 cpu = do
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu  
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu
--  (cpu,d) <- execCPU10 cpu
--  return (cpu,d)

-- ----------------------------------------------------------------- --
-- Memory emulation                                                  --
-- ----------------------------------------------------------------- --

memRead :: CPU -> Word16 -> IO Word8
memRead cpu adr
  | adr < 0x200    = return $ 0x00
  | adr < wFontTop = return $ (memRAM cpu) ! (adr - 0x200)
  | otherwise      = return $ memHEXFONT ! (adr - wFontTop) 

memWrite :: CPU -> Word16 -> Word8 -> IO CPU
memWrite cpu adr dat = do
  cpu <- return $ cpu { memRAM = (memRAM cpu) // [(adr - 0x200,dat)] }
  return cpu

-- ----------------------------------------------------------------- --
-- Load ROM image                                                    --
-- ----------------------------------------------------------------- --
-- loadRom :: CPU -> FilePath -> IO CPU
-- loadRom cpu fp = do
--   h <- openBinaryFile fp ReadMode
--   cpu <- loadRomAux cpu 0x0200 h
--   return cpu

-- loadRomAux :: CPU -> Word16 -> Handle -> IO CPU
-- loadRomAux cpu adr h = do
--   isEof <- hIsEOF h
--   cpu2 <- case isEof of
            -- 
--             False -> do
--               c <- hGetChar h
--               cpu3 <- memWrite cpu adr (c2w c)
--               printf "adr=0x%x dat=0x%x\n" adr (c2w c)
--               cpu4 <- loadRomAux cpu3 (adr+1) h
--               return cpu4
--             True -> do 
--               printf "adr=0x%x\n" adr
--               hClose h
--               return cpu
  --
--   return cpu2

-- End of Cpu.hs
