{-# LANGUAGE TemplateHaskell #-}
module Example where

import Control.Lens
import Codec.Picture.Types (Image, PixelRGBA8(..))
import Graphics.Rasterific
import Graphics.Rasterific.Texture

{-
alice = (triangle.colour ~= Yellow).border ~= Black
bob = (square.colour ~= Blue).border ~= Black
background = square.colour ~= Purple

scene = at (Seconds 2) $ (boing (Seconds 1) bob) <> at (Seconds 5) (boing (Seconds 1) alice) <> background

viewport is ((10, 10), (20, 20)) until viewport springs (Seconds 1) to ((0,0), (40,40)) after (Seconds 5)
-}

type Time = Float

class Tweenable a where
  -- |tween takes a starting value, and ending value and a time and produces a middle value  
  -- time is expected to always go between 0 and 1
  tween :: a -> a -> Time -> a

instance Tweenable Float where
  tween start end time = start + ((end - start) * time)

aTween :: Tweenable a => a -> a -> Animation a
aTween start end = Animation 1.0 $ tween start end

-- newtype Tween a = (Tweenable a) => a -> Time -> a
-- newtype Tween a = (Tweenable a) => Tween a -> 

-- takeTime :: (Tween a) => Time -> a -> a

--type Animation a = Time -> a
data Animation a = Animation Time (Time -> a)

-- |scaleTime takes a factor and make the animation take that factor of time
-- So, `scaleTime 2` will make it take twice as long, `scaleTime 0.5` will make it take half the time
scaleTime :: Float -> Animation a -> Animation a
scaleTime factor (Animation oldDur t) = Animation (oldDur * factor) $ \time -> t (time / factor)

-- |takeTime takes an animation and stretches or squeezes it as required to make it take the desired duration
takeTime :: Time -> Animation a -> Animation a
takeTime dur anim@(Animation oldDur _) = scaleTime (dur / oldDur) anim

pauseFor :: Time -> Animation a -> Animation a
pauseFor delay (Animation oldDur t) = Animation (delay + oldDur) $ \time -> if time < delay then t 0 else t (time - delay) 

holdFor delay (Animation oldDur t) = Animation (delay + oldDur) $ \time -> if time < oldDur then t time else t oldDur

-- |sampleAt takes a framerate and an animation and goes through the whole animation generating values for each frame
sampleAt :: Float -> Animation a -> [a]
sampleAt rate (Animation dur t) = map (\frame -> t $ (fromIntegral frame) / rate) [0..maxFrame]
  where
  maxFrame = floor (rate * dur) - 1

test :: [Rectangle]
test = sampleAt 1 $ holdFor 5 $ pauseFor 5 $ takeTime 5 $ moveRect ((2, 2), (4,4)) ((0,0), (6,6))
--test = let (Animation d f) = holdFor 5 $ pauseFor 5 $ takeTime 5 $ aTween 7 9 in [d, f 0]

-- anim = (viewport springs (Seconds 1) from ((10,10), (20,20)) to ((0,0), (40,40)))
-- this = pauseFor (Seconds 5) anim
-- that = holdFor (Seconds 5) this

-- holdFor (Seconds 5) $ pauseFor (Seconds 5) $ anim

instance (Tweenable a, Tweenable b) => Tweenable (a, b) where
  tween (s1, s2) (e1, e2) time = (tween s1 e1 time, tween s2 e2 time)

data Rectangle = Rectangle (Float, Float) (Float, Float) deriving (Show)

moveRect ((x0, y0), (x1, y1)) ((x2, y2), (x3, y3)) = Animation 1.0 $ \time -> Rectangle (tween x0 x2 time, tween y0 y2 time) (tween x1 x3 time, tween y1 y3 time)

data TestObj = TestObj { _size :: (Float, Float), _pos :: (Float, Float) } deriving (Show)

data TestScene = TestScene { _alice :: Maybe TestObj, _bob :: Maybe TestObj } deriving (Show)

-- testAnimation = takeTime 5 (alice is nothing) after (alice is (Just (0,0) (10, 10)) and alice.size goesFrom (0,0) (10,10))

after :: Animation a -> Animation a -> Animation a
after (Animation dur1 t1) (Animation dur2 t2) = Animation (dur1 + dur2) $ \time -> if time < dur1 then t1 time else t2 (time - dur1)

atT :: Time -> Animation (a -> a) -> Animation (a -> a)
atT timeToStart (Animation dur t) = Animation (dur + timeToStart) $ \time a -> if time < timeToStart then a else t (time - timeToStart) a

bothA :: Animation (a -> a) -> Animation (a -> a) -> Animation (a -> a)
bothA (Animation dur1 t1) (Animation dur2 t2) = Animation (max dur1 dur2) $ \time a -> t2 time $ t1 time a

animate :: Animation (a -> a) -> a -> Animation a
animate (Animation dur t) a = Animation dur $ \time -> t time a

testAnimation :: Animation TestScene
testAnimation = animate (bothA stuff things) defaultScene
  where
  stuff = atT 2 $ holdFor 2 $ Animation 1 $ \time a -> a{_alice = Just $ defaultAlice{_size = tween (0,0) (100,100) time}}
  things = atT 5 $ holdFor 2 $ Animation 1 $ \time a -> a{_bob = Just $ defaultBob{_size = tween (0,0) (100,100) time}}
  defaultScene = TestScene Nothing Nothing
  defaultAlice = TestObj (0,0) (10,10)
  defaultBob = TestObj (0,0) (130,80)

drawScene :: TestScene -> Image PixelRGBA8 
drawScene (TestScene alice bob) = renderDrawing 400 200 bg $ doAlice alice >> doBob bob
  where
  doAlice Nothing = return ()
  doAlice (Just (TestObj (w, h) (x,y))) = fillAndStroke aliceColour black $ rectangle (V2  x y) w h
  doBob Nothing = return ()
  doBob (Just (TestObj (w, h) (x,y))) = fillAndStroke bobColour black $ rectangle (V2  x y) w h
  black = PixelRGBA8 0 0 0 255
  aliceColour = PixelRGBA8 9 3 204 255
  bobColour = PixelRGBA8 204 50 2 255
  bg = PixelRGBA8 126 4 204 255

fillAndStroke fColor sColor shape = do
  withTexture (uniformTexture fColor) $ fill shape
  withTexture (uniformTexture sColor) $ stroke 4 (JoinMiter 0) (CapRound, CapRound) shape
  
