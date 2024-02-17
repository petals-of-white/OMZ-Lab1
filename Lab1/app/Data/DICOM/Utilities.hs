
module Data.DICOM.Utilities where

import           Data.Binary           as Binary (decode)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSChar
import           Data.ByteString.Lazy  as LBS (fromStrict)
import           Data.DICOM            as Dicom
import           Data.List             (delete, find)
import           Data.Word

type Intercept = Float
type Slope = Float
type Size = (Int, Int)
type BitsAllocated = Word16


getDicomData :: Object -> Maybe (Size, BS.ByteString, Intercept, Slope, BitsAllocated)
getDicomData dicom = do
      r :: Word16 <- decode . LBS.fromStrict . BS.reverse <$> findData Rows dicom
      c :: Word16 <- decode . LBS.fromStrict . BS.reverse <$> findData Columns dicom
      bitsAllocated :: BitsAllocated <- decode . LBS.fromStrict . BS.reverse <$> findData BitsAllocated dicom
      img <- findData PixelData dicom
      rescaleInter :: Float <- read. delete '+' . BSChar.unpack <$> findData RescaleIntercept dicom
      rescaleSlope :: Float <- read . delete '+' . BSChar.unpack <$> findData RescaleSlope dicom
      return ((fromIntegral r, fromIntegral c), img, rescaleInter, rescaleSlope, bitsAllocated)

findElement :: Tag -> Object -> Maybe Element
findElement t = find (\Element {elementTag = _t} -> _t == t) . runObject

findData :: Tag -> Object -> Maybe BS.ByteString
findData t o = findElement t o >>=
        (\Element {elementContent = content} -> case content of
            BytesContent bytesContent -> Just bytesContent
            _                         -> Nothing)
