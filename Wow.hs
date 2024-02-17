import Data.Bits
import Data.Word

-- Збереження значень кожного каналу
saveChannels :: Word32 -> (Word32, Word32, Word32, Word32)
saveChannels image = (alpha, red, green, blue)
  where
    alpha = (image `shiftR` 24) .&. 0xFF
    red   = (image `shiftR` 16) .&. 0xFF
    green = (image `shiftR` 8)  .&. 0xFF
    blue  = image .&. 0xFF

-- Зчитування значень кожного каналу
readChannels :: (Word32, Word32, Word32, Word32) -> Word32
readChannels (alpha, red, green, blue) =
  (alpha `shiftL` 24) .|. (red `shiftL` 16) .|. (green `shiftL` 8) .|. blue

main :: IO ()
main = do
  let originalImage = 0xABCD_EF01 :: Word32

  putStrLn $ "Original Image: " ++ show originalImage

  let channels@(a,r,g,b) = saveChannels originalImage
  putStrLn $ "Alpha: " ++ show (a)
  putStrLn $ "Red: " ++ show (r)
  putStrLn $ "Green: " ++ show (g)
  putStrLn $ "Blue: " ++ show (b)

  let reconstructedImage = readChannels channels
  putStrLn $ "Reconstructed Image: " ++ show reconstructedImage