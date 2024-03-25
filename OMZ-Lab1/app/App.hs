module App where
import           Codec.BMP                          (parseBMP)
import qualified Data.Binary                        as Binary
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.List.Split                    (chunksOf)
import           Graphics.Gloss                     (bitmapOfBMP)
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Image                     as Hip

import           ColorModel
import           LogicTrans
import           Types


worldToPic :: World -> Picture

worldToPic World {img, transformation} =
    case transformation of
        Original -> hipToGloss img

        LogicTrans op mask ->
            let maskImg = toWord8I $ toImageY (makeMaskImg mask (dims img)) in
            hipToGloss $ Hip.zipWith (bitwiseOp op) img maskImg

        ColourModel channel1 maybeChannel2 gradient ->
            hipToGloss $ colorModel channel1 maybeChannel2 gradient img



-- | Keyboard handling
handleInput :: (LogicOp, Mask) -> (Channel, Maybe Channel, Gradient) -> Event -> World -> World
handleInput (op, msk) (ch1, ch2, grad) event current =
    case event of
        (EventKey (Char key) Down _ _) ->
            case key of
                'o' -> current {transformation=Original}
                'l' -> current {transformation=LogicTrans op msk}
                'c' -> current {transformation=ColourModel ch1 ch2 grad}
                _   -> current
        _ -> current

-- | Convert bytestring (from dicom) to Hip Word8 image
imgFromPixData :: (Word16, Word16) -> BS.ByteString ->Image VS Y Word8

imgFromPixData sz@(_row, col) pixData  = fromLists matrixPixels
    where
        flatPixels = dicomPixDataToList sz pixData :: [Word8]
        matrixPixels = chunksOf (fromIntegral col) $ Prelude.map PixelY flatPixels



dicomPixDataToList :: Binary.Binary a => (Word16, Word16) -> BS.ByteString -> [a]

dicomPixDataToList (row, col) = Binary.decode . LBS.append (Binary.encode imgLen)  . LBS.fromStrict
    where imgLen = fromIntegral row * fromIntegral col :: Int


hipToGloss ::
    (Array arr cs e, Array arr RGB Double, ToRGB cs e, Writable (Image arr RGB Double) BMP) =>
    Image arr cs e -> Picture

hipToGloss hipImg =
    let bmp = either (error . show) id $ parseBMP $ encode BMP [] $ toImageRGB hipImg
    in bitmapOfBMP bmp
