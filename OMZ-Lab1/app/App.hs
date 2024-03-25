module App where

data Side = Left' | Top | Right' | Bottom deriving (Read, Show)

data Corner = TopLeft | TopRight | BottomRight | BottomLeft deriving (Read, Show)

data Mask = Rectangle Side | Triangle Corner deriving (Read, Show)

data LogicOp = And | Or deriving (Read, Show)

data Channel = R | G | B deriving (Read, Show)


data Mode = Original | LogicTrans LogicOp | ColourModel Channel (Maybe Channel) Gradient

data Gradient = Gradient deriving (Read, Show)
