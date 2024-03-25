module Main where

import           App
import           Data.DICOM           (readObjectFromFile)
import           Data.DICOM.Utilities
import           Data.Functor.Classes (readPrec1)
import           Graphics.Gloss
import           Graphics.Image       (normalize)
import           System.Environment   (getArgs)
import           Text.Read            (readEither, readMaybe)


main :: IO ()
main = do
    args <- getArgs
    let ProgramInput {} = either error id $ parseArgs args
    dicom <- either error id <$> readObjectFromFile dicomPath
    let elemMap = toMap dicom
        metadata = do
          r <- rows elemMap
          c <- columns elemMap
          intercept <- rescaleIntercept elemMap
          slope <- rescaleSlope elemMap
          bitsAlloc <- bitsAllocated elemMap
          imgBytes <- pixelData elemMap
          return (r,c, imgBytes, intercept, slope, bitsAlloc)

    case metadata of
      Right (row, col, imgBytes, _, _, _) -> do
            let (rowI, colI) = (fromIntegral row, fromIntegral col)

                AppImages {
                  original,
                  gauss,
                  prewitt,
                  weightedMedian
                } = hipToGloss . normalize <$> staticImgFromPixData (row, col) imgBytes sigma weights

                choosePic = handleMode original gauss prewitt weightedMedian

            play (InWindow "Lab3" (rowI, colI) (0, 0)) white 1 Original choosePic handleInput (const id)

      Left _ -> error "Error reading metadata!"



-- Cmd line argument parsing section
data ProgramInput = ProgramInput {
  dicomFile :: FilePath,
  logicOp   :: LogicOp,
  logicMask ::  Mask,
  channel   :: (Channel, Maybe Channel),
  gradient  :: Gradient
}

parseArgs :: [String] -> Either String ProgramInput
parseArgs [] = Left "No arguments"
parseArgs [dicom, op, maskType, maskPos, channels, grad] = do
  logicOp <- readEither "Wrong logic op" op
  mask <- readEither "Wrong mask" $ unwords [maskType, maskPos]
  chan <- case channels of
    [singleCh] -> (\ch -> (ch, Nothing)) <$> readEither ("Unrecognized channel: " ++ show singleCh) singleCh
    [fstCh, sndCh] -> do
        first  <-  readEither ("Unrecognized channel: " ++ show fstCh) fstCh
        second <- readEither ("Unrecognized channel: " ++ show sndCh) sndCh
        return (first, Just second)
  gr <- readEither "Wrong gradient" grad
  return ProgramInput {
    dicomFile = dicom,
    logicOp = logicOp,
    logicMask = mask,
    channel = chan,
    gradient = gr
  }
parseArgs _ = Left "Wrong args"
-- parseArgs (dicom:sigma:weights) = do
--   sigm <- maybeToEither "Error reading sigma" $ readMaybe sigma
--   kernel <- maybeToEither "Error reading kernel" $ readMaybe $ unwords weights
--   return ProgramInput {dicomFile=dicom, medianWeights=kernel, sigma=sigm}
