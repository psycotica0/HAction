{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.HAction

import Control.Monad (forM_)
import Codec.Picture (writePng)
import Codec.Picture.Types (Image, PixelRGBA8(..))
import System.Environment (getArgs)
import Control.Lens
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations (translate, scale)
import Data.Monoid ((<>))

data TestObj = TestObj { _size :: (Float, Float), _pos :: (Float, Float) } deriving (Show)
makeLenses ''TestObj

data UnsavoryCharacters = UnsavoryCharacters { _u1 :: Maybe TestObj, _u2 :: Maybe TestObj, _u3 :: Maybe TestObj, _u4 :: Maybe TestObj, _u5 :: Maybe TestObj, _u6 :: Maybe TestObj } deriving (Show)
makeLenses ''UnsavoryCharacters

data TestScene = TestScene { _viewport :: ((Float, Float), (Float, Float)), _alice :: Maybe TestObj, _bob :: Maybe TestObj, _unsavory :: UnsavoryCharacters} deriving (Show)
makeLenses ''TestScene

popIn lns obj finalSize = bothA appear grow
  where
  appear = pure $ lns .~ Just obj
  grow = hold $ elastic $ (lns . _Just . size) .~% finalSize

animateUnsavory :: Animation (UnsavoryCharacters -> UnsavoryCharacters)
animateUnsavory = allA [
      unsavoryPop u1 (181, 79),
      atT 0.25 $ unsavoryPop u2 (128, 112),
      atT 0.4 $ unsavoryPop u3 (245, 74),
      atT 0.6 $ unsavoryPop u4 (202, 147),
      atT 0.7 $ unsavoryPop u5 (265, 117),
      atT 0.75 $ unsavoryPop u6 (215, 109)
    ]
  where
  unsavoryPop lns pos = popIn lns (TestObj (0,0) $ shiftPos pos) (75, 75)
  -- This inverts the viewport computation, because I figured out the positions
  -- of these things by pointing on the rendered image
  -- A little leaky, but in the end the computer doesn't care
  -- Doing this by hand to get arbitrary numbers to put in here doesn't win me
  -- anything
  shiftPos (x, y) = (between (x / 400) (-600) 1000, between (y / 200) (-300) 500)

testAnimation :: Animation TestScene
testAnimation = animate anims defaultScene
  where
  anims = holdFor 1 $ allA [
    -- Alice appears at 2
    atT 2 $ popIn alice defaultAlice (100, 100),
    -- Bob appears at 5
    atT 5 $ popIn bob defaultBob (100, 100),
    atT 4 $ hold $ quint $ (alice ._Just .pos) .~% (35, 35),
    atT 8 $ hold $ quint $ viewport .~% ((-600, -300), (1000, 500)),
    atT 10 $ hold $ quint $ allA [
      (alice . _Just . pos) .~% (-500, -200),
      (bob . _Just . pos) .~% (900, 400)
      ],
    atT 12 $ over unsavory <$> animateUnsavory
    ]
  defaultScene = TestScene ((0,0), (400,200)) Nothing Nothing defaultUnsavory
  defaultUnsavory = UnsavoryCharacters Nothing Nothing Nothing Nothing Nothing Nothing
  defaultAlice = TestObj (0,0) (70,70)
  defaultBob = TestObj (0,0) (200,100)

black = PixelRGBA8 0 0 0 255

drawUnsavory (UnsavoryCharacters u1 u2 u3 u4 u5 u6) = do
    forM_ [u1, u2, u3, u4, u5, u6] $ \u -> do
      mapM_ (shadow . rect) u
      mapM_ (fillAndStroke unsavoryColour black . rect) u
  where
  unsavoryColour = PixelRGBA8 131 182 107 255

drawScene :: TestScene -> Image PixelRGBA8 
drawScene (TestScene view alice bob unsavory) = renderDrawing width height bg $ withViewport $ do
    mapM_ (shadow . rect) alice
    mapM_ (fillAndStroke aliceColour black . rect) alice
    mapM_ (shadow . triangle) bob
    mapM_ (fillAndStroke bobColour black . triangle) bob
    drawUnsavory unsavory
  where
  width = height * 2
  height = 200
  ((vx1, vy1), (vx2, vy2)) = view
  viewportTransform = scale (fromIntegral width / (vx2 - vx1)) (fromIntegral height / (vy2 - vy1)) <> translate (V2 (-vx1) (-vy1))
  withViewport = withTransformation viewportTransform
  aliceColour = PixelRGBA8 9 3 204 255
  bobColour = PixelRGBA8 204 50 2 255
  bg = PixelRGBA8 126 4 204 255

rect (TestObj (w, h) (x, y)) = rectangle (V2 (x - w/2) (y - h/2)) w h
triangle (TestObj (w, h) (x, y)) = polygon [V2 (x - w/2) (y + h/2), V2 x (y - h/2), V2 (x + w/2) (y + h/2)]

shadow shape = withTexture (uniformTexture shadowColour) $ withTransformation (scale 1.1 1.1) $ fill shape
  where
  shadowColour = PixelRGBA8 0 0 0 128

fillAndStroke fColor sColor shape = do
  withTexture (uniformTexture fColor) $ fill shape
  withTexture (uniformTexture sColor) $ stroke 2 (JoinMiter 0) (CapRound, CapRound) shape

main :: IO ()
main = do
  let frameRate = 24
  let frames = zip [0..] $ map drawScene $ sampleAt frameRate testAnimation
  args <- getArgs
  case args of
    -- A particular second was requested
    x:_ -> let
      fr = floor (frameRate * read x)
      (idx, img) = frames !! fr
      in
      writePng ("frameT" ++ x ++ ".png") img
    otherwise -> forM_ frames $ \(idx, img) -> writePng ("frame" ++ show idx ++ ".png") img
