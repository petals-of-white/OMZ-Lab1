module Main where

import           App
import           Data.DICOM           (readObjectFromFile)
import           Data.DICOM.Utilities
import           Graphics.Gloss
import           System.Environment   (getArgs)
import           Text.Read            (readMaybe)
import           Types


main :: IO ()
main = do
    args <- getArgs
    let ProgramInput {
      dicomFile=dicomPath,
      logicOp,
      logicMask,
      channel,
      gradient
    } = either error id $ parseArgs args
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
                init_ = World {img = imgFromPixData (row, col) imgBytes, transformation = Original}

            play (InWindow "Lab1" (rowI, colI) (0, 0)) white 1 init_ worldToPic
              (handleInput (logicOp, logicMask) (fst channel, snd channel, gradient))
              (const id)

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
  logicOp <- maybeToEither "Wrong logic op"  $ readMaybe op
  mask <- maybeToEither "Wrong mask" $ readMaybe $ unwords [maskType, maskPos]
  chan <- case channels of
    [singleCh] ->

      (\ch -> (ch, Nothing)) <$> maybeToEither ("Unrecognized channel: " ++ show singleCh) (readMaybe [singleCh])

    [fstCh, sndCh] -> do
        first  <-  maybeToEither ("Unrecognized channel: " ++ show fstCh) $ readMaybe [fstCh]
        second <- maybeToEither ("Unrecognized channel: " ++ show sndCh) $ readMaybe [sndCh]
        return (first, Just second)

    _ -> Left "Unrecognized channels. Length should be 1 or "

  gr <- maybeToEither "Wrong gradient" $ readMaybe grad

  return ProgramInput {
    dicomFile = dicom,
    logicOp = logicOp,
    logicMask = mask,
    channel = chan,
    gradient = gr
  }

parseArgs _ = Left "Wrong args"
