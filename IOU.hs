-- ----------------------------------------------------------------- --
--                                                                   --
--  MonadChip8.hs : CHIP-8 Emulator                                  --
--                                                                   --
--  2016/09/28  Jay Kumogata(twitter: jay1905)                       --
--                                                                   --
-- ----------------------------------------------------------------- --

module IOU (
  IOU,
  initIOU,
  ioDelay,
  ioSound,
  ioKey,
  ioEsc,
) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Array
import Control.Monad
import Text.Printf
import System.IO
   
-- ------------------------------------------------------------ --
--   I/O Resources                                              --
-- ------------------------------------------------------------ --

data IOU = IOU {
  -- Delay Timer
    ioDelay :: Word8
  -- Sound Timer
  , ioSound :: Word8
  -- Key State : FEDCBA9876543210
  , ioKey :: Word16
  , ioEsc :: Word8
  }

-- ----------------------------------------------------------------- --
-- Initialze IOU                                                     --
-- ----------------------------------------------------------------- --

initIOU :: IO IOU
initIOU = do
  return IOU {
  -- Delay Timer
    ioDelay = 0x00
  -- Sound Timer
  , ioSound = 0x00
  -- Key State : FEDCBA9876543210
  , ioKey = 0x0000
  , ioEsc = 0x00
  }

-- ----------------------------------------------------------------- --
-- Show state of IOU                                                 --
-- ----------------------------------------------------------------- --

instance Show IOU where
  show iou = unlines $ [reg8,reg16 ] where
    reg8 = concatMap (showReg 2) [("Delay", ioDelay ),
                                  ("Sound", ioSound ),
                                  ("Esc", ioEsc )] 
    reg16 = concatMap (showReg 4) [("Key", ioKey)]

    showReg n (s,f) = s ++ " : 0x" ++ toHex n (toEnum $ fromEnum $ f iou) ++ " "

toHex :: Int -> Int -> String
toHex n = reverse . map ((tbl!!).(`mod`16)) . take n . iterate (`div`16)
  where tbl="0123456789ABCDEF"

-- End of IOU.hs
