module Example where

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

test :: [Float]
test = sampleAt 1 $ holdFor 5 $ pauseFor 5 $ takeTime 5 $ aTween 7 9
--test = let (Animation d f) = holdFor 5 $ pauseFor 5 $ takeTime 5 $ aTween 7 9 in [d, f 0]

-- anim = (viewport springs (Seconds 1) from ((10,10), (20,20)) to ((0,0), (40,40)))
-- this = pauseFor (Seconds 5) anim
-- that = holdFor (Seconds 5) this

-- holdFor (Seconds 5) $ pauseFor (Seconds 5) $ anim
