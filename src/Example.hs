{-# LANGUAGE TemplateHaskell #-}
module Example where

import Control.Lens
import Codec.Picture.Types (Image, PixelRGBA8(..))
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations (translate, scale)
import Data.Monoid ((<>))
import Control.Monad (forM_)

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
-- scaleTime factor = tmap $ \(dur, time) -> (dur * factor, time / factor)

-- |takeTime takes an animation and stretches or squeezes it as required to make it take the desired duration
takeTime :: Time -> Animation a -> Animation a
takeTime dur anim@(Animation oldDur _) = scaleTime (dur / oldDur) anim

pauseFor :: Time -> Animation a -> Animation a
pauseFor delay (Animation oldDur t) = Animation (delay + oldDur) $ \time -> if time < delay then t 0 else t (time - delay) 
-- pauseFor delay = tmap $ \(dur, time) -> (dur + delay, if time < delay then 0 else time - delay)

holdFor delay (Animation oldDur t) = Animation (delay + oldDur) $ \time -> if time < oldDur then t time else t oldDur
-- holdFor delay = tmap $ \(dur, time) -> (delay + dur, min dur time)
-- hold = holdFor 0
-- hold = tmap (max 1)

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
makeLenses ''TestObj

data UnsavoryCharacters = UnsavoryCharacters { _u1 :: Maybe TestObj, _u2 :: Maybe TestObj, _u3 :: Maybe TestObj, _u4 :: Maybe TestObj, _u5 :: Maybe TestObj, _u6 :: Maybe TestObj } deriving (Show)

makeLenses ''UnsavoryCharacters
data TestScene = TestScene { _viewport :: ((Float, Float), (Float, Float)), _alice :: Maybe TestObj, _bob :: Maybe TestObj, _unsavory :: UnsavoryCharacters} deriving (Show)
makeLenses ''TestScene

-- testAnimation = takeTime 5 (alice is nothing) after (alice is (Just (0,0) (10, 10)) and alice.size goesFrom (0,0) (10,10))

after :: Animation a -> Animation a -> Animation a
after (Animation dur1 t1) (Animation dur2 t2) = Animation (dur1 + dur2) $ \time -> if time < dur1 then t1 time else t2 (time - dur1)

atT :: Time -> Animation (a -> a) -> Animation (a -> a)
atT timeToStart (Animation dur t) = Animation (dur + timeToStart) $ \time a -> if time < timeToStart then a else t (time - timeToStart) a

instance Functor Animation where
  fmap f (Animation dur t) = Animation dur $ fmap f t

-- |tmap_n is my profunctor, but I can't be a Profunctor
-- It maps functions from (Time -> Time) which I use for many of my warps
-- For the purposes of the inner function, the animation always goes from 0 to 1
-- (It's normalized to duration by tmap)
-- It's expected to produce a new value between 0 and 1 as well, which will be
--   denormalized back into duration
tmap_n :: (Time -> Time) -> Animation a -> Animation a
tmap_n f (Animation dur t) = Animation dur $ t . (dur *) . f . (/ dur)
-- tmap_n f = tmap $ \(dur, time) -> (dur, dur * (f (time / dur)))

hold = tmap_n (min 1)

{-
  There are many Applicative instances I could have chosen when the duration
  differ
  - Allow both sides to be elastic: The shorter one explodes beyond its
    duration
  - Allow longer side to be elastic: The shorter is capped at its duration,
    longer expands
  - Only be the length of the shorter: The longer side is only expressed by
    explotion
  - Stretch both to be the longer length: Both remain elastic, and shorter is
    scaled to match longer

  I've gone with "Allow both sides to be elastic" because it's simpler to
  execute and most flexible
  If you want one side to not be elastic, you can always use `hold`

  It does mean in practical use you may be putting hold everywhere.
  I was worried about running equality on Floats, and guessing wrong what
  people were hoping for, so it's explicit

  The others options are totally viable, though

-}
instance Applicative Animation where
  pure = Animation 0 . const
  (Animation dur1 f) <*> (Animation dur2 g) = Animation (max dur1 dur2) $ \time -> f time $ g time

-- |This is mempty, but Haskell won't let me do it without extentions
emptyA :: Animation (a -> a)
emptyA = pure id -- Animation 0 $ const id

-- |This is mappend, but Haskell won't let me do it without extensions
bothA :: Animation (a -> a) -> Animation (a -> a) -> Animation (a -> a)
--bothA (Animation dur1 t1) (Animation dur2 t2) = Animation (max dur1 dur2) $ \time a -> t2 time $ t1 time a
bothA a1 a2 = pure (.) <*> a2 <*> a1

-- |This is mconcat, but Haskell won't let me do it without extensions
allA :: [Animation (a -> a)] -> Animation (a -> a)
allA = foldl bothA emptyA

animate :: Animation (a -> a) -> a -> Animation a
-- animate (Animation dur t) a = Animation dur $ \time -> t time a
-- animate anim def = fmap ($ def) anim
animate anim def = anim <*> pure def

(.~~.) :: Tweenable b => ASetter s t a b -> b -> b -> Time -> s -> t
(.~~.) lns v1 v2 time obj = set lns (tween v1 v2 time) obj

(.~~) :: Tweenable b => ASetter s s a b -> b -> b -> Animation (s -> s)
(.~~) lns v1 v2 = Animation 1 $ \time -> set lns (tween v1 v2 time)

(.~%) lns v2 = Animation 1 $ \time -> over lns (\v1 -> tween v1 v2 time)

doA :: (a -> a) -> Animation (a -> a)
-- doA f = Animation 1 $ \_ -> f
doA = pure

-- Stolen from https://joshondesign.com/2013/03/01/improvedEasingEquations
elastic = tmap_n innerElastic
  where
  innerElastic t = (2 ** (-10 * t)) * sin((t-p/4)*(2*pi)/p) + 1
  p = 0.3

quint = tmap_n $ \time -> 1 - ((1 - time) ** 5)

popIn lns obj finalSize = bothA appear grow
  where
  appear = doA $ lns .~ Just obj
  grow = hold $ elastic $ (lns . _Just . size) .~% finalSize

-- |between is just a semantic convenience
-- It finds a value 0.5 of the way between two values, for example.
-- That's also what tween does at second 0.5
-- But I don't want to put tween everywhere, because it'll look like I'm
--   animating things I'm not
-- So I use between for "static" values
between factor v1 v2 = tween v1 v2 factor

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
  width = 400
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
  
