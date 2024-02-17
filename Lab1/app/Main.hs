{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where
import           Control.Monad                     (unless)
import           Control.Monad.IO.Class
import           Data.Binary                       as Binary (decode, encode)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Char8             as BSChar
import           Data.ByteString.Lazy              as LBS (append, fromStrict)
import           Data.DICOM                        as Dicom
import           Data.List                         (delete, find)
import           Data.Maybe                        (fromJust)
import           Data.Word                         (Word16, Word8, Word32)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW       (WindowConfig (..))
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import           Graphics.GPipe.Context.GLFW.Input
import           Paths_OMZ_Lab1                    (getDataFileName)
import           Prelude                           hiding (reverse)
import Data.Int (Int32)

main :: IO ()
main = do
  filename <- getDataFileName "DICOM_Image_8b.dcm"
  dicom <- either error id <$> readObjectFromFile filename
  let ((rows, columns), imgBytes, intercept, slope, bitsAlloc) = fromJust $ getDicomData dicom
      size = rows * columns
      imgWord8 :: [Word8] = (decode . LBS.append (encode size) . fromStrict) imgBytes
  putStrLn $
    "Rows: " ++ show rows ++ ". Columns: " ++ show columns ++ ". Intercept: "
    ++ show intercept ++ ". Slope: " ++ show slope ++ ". Bits allocated: " ++ show bitsAlloc

  case (intercept, slope, bitsAlloc) of
    (i, s, _) | i /= 0, s /= 0 -> putStrLn "gl float"
    (_,_, 8)                   -> putStrLn "gl byte"
    (_,_, 16)                  -> putStrLn "gl short"
    _                          -> error "Unrecognized type?"


  runContextT GLFW.defaultHandleConfig $ do
    -- buffer
    vertexBuffer1 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
    vertexBuffer2 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
    vertexBufferMask :: Buffer os (B2 Float, B Word32) <- newBuffer 12

    -- задамо у вершини у вигляді координата, uv-текстури
    writeBuffer vertexBuffer1 0 [(V2 (-1) (-1), V2 0 1),  (V2 1 (-1), V2 1 1),
                               (V2 (-1) 1, V2 0 0),     (V2 1 1, V2 1 0)]

    writeBuffer vertexBuffer2 0 [(V2 (-1) (-1), V2 0 0),  (V2 1 (-1), V2 1 0),
                               (V2 (-1) 1, V2 0 1),     (V2 1 1, V2 1 1)]

    writeBuffer vertexBufferMask 0
      [(V2 (-1) (-1), 255), (V2 (-1) 1, 255), (V2 0 (-1), 255),
       (V2 0 (-1),255),     (V2 0 1, 255), (V2 (-1) 1, 255),
       (V2 0 (-1),0),       (V2 0 1,0),       (V2 1 (-1),0),
       (V2 1 (-1),0),       (V2 1 1,0),       (V2 0 1,0)]

    -- writeBuffer vertexBufferMask 0
    --   [(V2 (-1) (-1), maxBound), (V2 (-1) 1, maxBound), (V2 0 (-1), maxBound),
    --   (V2 0 (-1),maxBound),     (V2 0 1, maxBound), (V2 (-1) 1, maxBound),
    --   (V2 0 (-1),0),       (V2 0 1,0),       (V2 1 (-1),0),
    --   (V2 1 (-1),0),       (V2 1 1,0),       (V2 0 1,0)]

    -- Textures
    let texSize = V2 rows columns
    originalTex <- newTexture2D R8UI texSize 1
    logicTex <- newTexture2D R8UI texSize 1

    writeTexture2D originalTex 0 0 texSize imgWord8
    writeTexture2D logicTex 0 0 texSize imgWord8

    win <- newWindow (WindowFormatColor RGB8) ((GLFW.defaultWindowConfig "Dicom Test") {configWidth=rows, configHeight=columns})



    -- Shaders
    shaderMask <- compileShader $ do
      primitiveStream <- toPrimitiveStream primitives

      let primitiveStream2 = fmap (\(V2 x y, c) -> (V4 x y 0 1, c)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2

      draw (const (LogicOp And)) fragmentStream $ \c ->
        drawColor (\ s -> (colorImage s, True, True)) c



    shaderTexColor <- compileShader $ do
      primitiveStream  <- toPrimitiveStream id

      let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2

      let filter = SamplerNearest
          edge = (pure ClampToEdge, undefined)
      samp <- newSampler2D (const (originalTex, filter, edge))

      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          -- перетворимо word8 на float
          fragmentStream2 = fmap (((/ 255) . fmap toFloat) . (\g -> V3 0 g 0) . sampleTexture) fragmentStream

      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2


    {-
    shaderBlend <- compileShader $ do
      primitiveStream <- toPrimitiveStream primitives
      let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2
      let --filter = SamplerFilter Nearest Nearest Nearest Nothing
          filter = SamplerNearest
          edge = (pure Repeat, undefined)
      samp <- newSampler2D (const (originalTex, filter, edge))

      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          fragmentStreamSampled = fmap sampleTexture fragmentStream

      draw (const (LogicOp And)) fragmentStreamSampled $ \a ->
        drawColor (\ s -> (colorImage s, True, True)) a

    -}
    shaderWord8TexToWin <- compileShader $ do
      primitiveStream  <- toPrimitiveStream fst

      let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2

      let filter = SamplerNearest
          edge = (pure ClampToEdge, undefined)
      samp <- newSampler2D (\(_,targetTex) -> (targetTex, filter, edge))
      let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing
          -- перетворимо word8 на float
          fragmentStream2 = fmap (((/ 255) . fmap toFloat) . sampleTexture) fragmentStream

      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

    render $ do
        rectangleVertArray <- newVertexArray vertexBufferMask
        cImage <- getTexture2DImage logicTex 0
        -- clearImageColor cImage 
        shaderMask $ ShaderEnvironment
          (toPrimitiveArray TriangleList rectangleVertArray)
          cImage

    renderLoop win Renderings {
      renderOriginal = do
        vertexArray <- newVertexArray vertexBuffer1
        shaderWord8TexToWin (toPrimitiveArray TriangleStrip vertexArray, originalTex)
        ,

      renderLogic = do
        vertexArray <- newVertexArray vertexBuffer1
        shaderWord8TexToWin (toPrimitiveArray TriangleStrip vertexArray, logicTex)
        ,

      renderColor = do
        clearWindowColor win 0
        vertexArray <- newVertexArray vertexBuffer1
        shaderTexColor (toPrimitiveArray TriangleStrip vertexArray)
    }
    {-
      [

      do
        rectangleVertArray <- newVertexArray vertexBufferMask
        cImage <- getTexture2DImage targetTex 0
        clearImageColor cImage 0
        shaderMask $ ShaderEnvironment
          (toPrimitiveArray TriangleStrip rectangleVertArray)
          cImage
      ,

      do
        vertexArray <- newVertexArray vertexBuffer1
        cImage <- getTexture2DImage targetTex 0
        shaderBlend $ ShaderEnvironment
          (toPrimitiveArray TriangleStrip vertexArray)
          cImage
      ,

      do
        clearWindowColor win 0
        vertexArray <- newVertexArray vertexBuffer2
        shaderWord8TexToWin (toPrimitiveArray TriangleStrip vertexArray)
      ]
      -}


renderLoop win renderings@(Renderings rOrig rLogic rColor)  = do

  logic <- getKey win Key'L
  colorModelling <- getKey win Key'C
  liftIO (putStrLn "Rendering orig")

  case (logic, colorModelling) of
    (Just KeyState'Pressed, _) -> liftIO (putStrLn "Rendering logic..") >> render rLogic
    (_, Just KeyState'Pressed) -> liftIO (putStrLn "Rendering color ..") >> render rColor
    -- -> liftIO $ putStrLn $ "Both:" ++ show l ++ " " ++ show c
    _ -> render rOrig



  -- mapM_ render renderings
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    renderLoop win renderings

data ShaderEnvironment a = ShaderEnvironment
  {
    primitives :: PrimitiveArray Triangles a,
    colorImage :: Image (Format RWord)
  }


data Renderings os  = Renderings {
  renderOriginal :: Render os (),
  renderLogic    :: Render os (),
  renderColor    :: Render os ()
  }

-- Data extraction
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
