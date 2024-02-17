module Main where
import           Control.Monad                     (unless)
import           Control.Monad.Exception           (MonadException)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Binary                       as Binary (decode, encode)
import           Data.ByteString.Lazy              as LBS (append, fromStrict)
import           Data.DICOM                        as Dicom
import           Data.DICOM.Utilities
import           Data.Maybe                        (fromJust)
import           Data.Word                         (Word32, Word8)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW       (WindowConfig (..))
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import           Graphics.GPipe.Context.GLFW.Input
import           Paths_OMZ_Lab1                    (getDataFileName)
import           Prelude                           hiding (reverse)

main :: IO ()
main =
  let logicOp = LogicOp And
      transformColor green = V3 0 green 0 in
  do
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

    -- Textures
    let texSize = V2 rows columns
    originalTex <- newTexture2D R8UI texSize 1
    logicTex <- newTexture2D R8UI texSize 1

    writeTexture2D originalTex 0 0 texSize imgWord8
    writeTexture2D logicTex 0 0 texSize imgWord8

    win <- newWindow (WindowFormatColor RGB8) ((GLFW.defaultWindowConfig "Lab1") {configWidth=rows, configHeight=columns})

    -- Shaders
    shaderMask <- compileShader $ do
      primitiveStream <- toPrimitiveStream primitives

      let primitiveStream2 = fmap (\(V2 x y, c) -> (V4 x y 0 1, c)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) texSize, DepthRange 0 1)) primitiveStream2

      draw (const logicOp) fragmentStream $ \c ->
        drawColor (\ s -> (colorImage s, True, True)) c

    shaderTexColor <- compileShader $ do
      primitiveStream  <- toPrimitiveStream id

      let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) texSize, DepthRange 0 1)) primitiveStream2

      let edge = (pure ClampToEdge, undefined)
      samp <- newSampler2D (const (originalTex, SamplerNearest, edge))

      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          -- перетворимо word8 на float
          fragmentStream2 = fmap (((/ 255) . fmap toFloat) . transformColor . sampleTexture) fragmentStream

      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

    shaderWord8TexToWin <- compileShader $ do
      primitiveStream  <- toPrimitiveStream fst

      let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2

      let edge = (pure ClampToEdge, undefined)
      samp <- newSampler2D (\(_,targetTex) -> (targetTex, SamplerNearest, edge))
      let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing
          -- перетворимо word8 на float
          fragmentStream2 = fmap (((/ 255) . fmap toFloat) . sampleTexture) fragmentStream

      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

    render $ do
        rectangleVertArray <- newVertexArray vertexBufferMask
        cImage <- getTexture2DImage logicTex 0
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


renderLoop :: (MonadIO m,  MonadException m) => Window os c ds -> Renderings os -> ContextT GLFW.Handle os m ()
renderLoop win renderings@(Renderings rOrig rLogic rColor)  = do
  logic <- getKey win Key'L
  colorModelling <- getKey win Key'C
  case (logic, colorModelling) of
    (Just KeyState'Pressed, _) -> render rLogic
    (_, Just KeyState'Pressed) -> render rColor
    _                          -> render rOrig

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