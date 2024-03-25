module LogicTrans where

import           Control.Applicative (Applicative (liftA2))
import           Data.Bits           (Bits ((.|.)), (.&.))
import           Graphics.Image
import           Types

makeMaskImg :: Mask -> (Int, Int) -> Image VS X Bit
makeMaskImg mask size@(row, col)  =
    makeImage size generator
    where
        generator =
            case mask of
                Rectangle Left' -> (\(_,c) -> if c <= col `div` 2 then on else off)
                Rectangle Top -> (\(r,_) -> if r <= row `div` 2 then on else off)
                Rectangle Right' -> (\(_,c) -> if c > col `div` 2 then on else off)
                Rectangle Bottom -> (\(r,_) -> if r > row `div` 2 then on else off)

                Triangle TopLeft -> (\(r,c) -> if (c + r) <= col then on else off)
                Triangle TopRight -> (\(r,c) -> if (c + r) > col then on else off)
                Triangle BottomRight -> (\(r,c) -> if r <= c  then on else off)
                Triangle BottomLeft -> (\(r,c) -> if r > c then on else off)


bitwiseOp :: LogicOp -> Pixel Y Word8 -> Pixel Y Word8 -> Pixel Y Word8
bitwiseOp And = liftA2 (.&.)
bitwiseOp Or  = liftA2 (.|.)
