module Types where
import           Graphics.Image (Image, VS, Word8, Y)


data Side = Left' | Top | Right' | Bottom deriving (Read, Show)

data Corner = TopLeft | TopRight | BottomRight | BottomLeft deriving (Read, Show)

data Mask = Rectangle Side | Triangle Corner deriving (Read, Show)

data LogicOp = And | Or deriving (Read, Show)

data Channel = R | G | B deriving (Read, Show)

data TransMode = Original | LogicTrans LogicOp Mask | ColourModel Channel (Maybe Channel) Gradient

data Gradient = LeftToRight  | RightToLeft | Middle | Corners  deriving (Read, Show)

data World = World {img :: Image VS Y Word8, transformation :: TransMode}
