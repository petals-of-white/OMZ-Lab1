module ColorModel where
import           Graphics.Image           as Hip
import           Graphics.Image.Interface (ColorSpace (getPxC))
import           Types


-- | Linear interpolation between 2 fractionals
interpolateLin2 :: Fractional a => a -> a -> Float -> a

interpolateLin2 col1 col2 fraction = (col2 - col1) * realToFrac fraction + col1


-- | Symmetric interpolation ('mountain')
interpolateMiddle :: Fractional a => a -> a -> Float -> a

interpolateMiddle col1 col2 fraction = (col2 - col1) * realToFrac (abs (0.5 - fraction) * 2) + col1



colorModel :: Channel -> Maybe Channel -> Gradient -> Image VS Y Word8 -> Image VS RGB Word8

colorModel ch1 ch2 grad = Hip.map (fmap round . interpol . flip getPxC LumaY) . toFloatI
    where
        col1 = fromIntegral <$> chanToRGB ch1 :: Pixel RGB Float
        col2 =  fromIntegral <$> maybe 0 chanToRGB ch2
        interpol =
            case grad of
                LeftToRight -> interpolateLin2 col1 col2
                RightToLeft -> interpolateLin2 col2 col1
                Middle      -> interpolateMiddle col1 col2
                Corners     -> interpolateMiddle col2 col1

        chanToRGB :: Channel -> Pixel RGB Word8
        chanToRGB R = PixelRGB maxBound 0 0
        chanToRGB G = PixelRGB 0 maxBound 0
        chanToRGB B = PixelRGB 0 0 maxBound
