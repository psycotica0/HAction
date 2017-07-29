module Graphics.HAction where

import Control.Lens

type Time = Float

class Tweenable a where
  -- |tween takes a starting value, and ending value and a time and produces a middle value  
  -- time is expected to always go between 0 and 1
  tween :: a -> a -> Time -> a

data Animation a = Animation Time (Time -> a)

instance Tweenable Float where
  tween start end time = start + ((end - start) * time)

aTween :: Tweenable a => a -> a -> Animation a
aTween start end = Animation 1.0 $ tween start end

-- |scaleTime takes a factor and make the animation take that factor of time
-- So, `scaleTime 2` will make it take twice as long, `scaleTime 0.5` will make it take half the time
scaleTime :: Float -> Animation a -> Animation a
scaleTime factor (Animation oldDur t) = Animation (oldDur * factor) $ \time -> t (time / factor)
-- scaleTime factor = tmap $ \(dur, time) -> (dur * factor, time / factor)

-- |takeTime takes an animation and stretches or squeezes it as required to make it take the desired duration
takeTime :: Time -> Animation a -> Animation a
takeTime dur anim@(Animation oldDur _) = scaleTime (dur / oldDur) anim

-- |pauseFor delays the start of an animation by the amount of time given, filling the delay with the animation at time 0
pauseFor :: Time -> Animation a -> Animation a
pauseFor delay (Animation oldDur t) = Animation (delay + oldDur) $ \time -> if time < delay then t 0 else t (time - delay) 
-- pauseFor delay = tmap $ \(dur, time) -> (dur + delay, if time < delay then 0 else time - delay)

-- |holdFor takes an animation and extends the end of the animation for the given amount of time, holding the animation as rendered at its duration
holdFor delay (Animation oldDur t) = Animation (delay + oldDur) $ \time -> if time < oldDur then t time else t oldDur
-- holdFor delay = tmap $ \(dur, time) -> (delay + dur, min dur time)

-- |sampleAt takes a framerate and an animation and goes through the whole animation generating values for each frame
sampleAt :: Float -> Animation a -> [a]
sampleAt rate (Animation dur t) = map (\frame -> t $ (fromIntegral frame) / rate) [0..maxFrame]
  where
  maxFrame = floor (rate * dur) - 1

instance (Tweenable a, Tweenable b) => Tweenable (a, b) where
  tween (s1, s2) (e1, e2) time = (tween s1 e1 time, tween s2 e2 time)

-- |after takes two animations on the same type and produces a composite animation that does one to completion, then the other
after :: Animation a -> Animation a -> Animation a
after (Animation dur1 t1) (Animation dur2 t2) = Animation (dur1 + dur2) $ \time -> if time < dur1 then t1 time else t2 (time - dur1)

-- |atT takes an animating modifier and produces a new modifier that does nothing up until the time provided, then applies the modification for the duration afterwards.
-- `pauseFor` delays an animation by filling the empty space with the result at time 0
-- `atT` doesn't apply this function at all until after the delay
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

-- |hold turns an elastic animation into a non-elastic one.
-- It caps an animation at its duration so that its final moment is frozen from then on
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

-- |empty is mempty, but Haskell won't let me do it without extentions
emptyA :: Animation (a -> a)
emptyA = pure id -- Animation 0 $ const id

-- |bothA is mappend, but Haskell won't let me do it without extensions
bothA :: Animation (a -> a) -> Animation (a -> a) -> Animation (a -> a)
bothA a1 a2 = pure (.) <*> a2 <*> a1

-- |allA is mconcat, but Haskell won't let me do it without extensions
allA :: [Animation (a -> a)] -> Animation (a -> a)
allA = foldl bothA emptyA

-- |animate takes an animated modifier, and turns it into an animation by supplying an initial object to modify
animate :: Animation (a -> a) -> a -> Animation a
animate anim def = anim <*> pure def

-- |(.~~.) is a lens setter that tweens from one value to another over time
(.~~.) :: Tweenable b => ASetter s t a b -> b -> b -> Time -> s -> t
(.~~.) lns v1 v2 time obj = set lns (tween v1 v2 time) obj

-- |(.~~) produces an animated modifier that modifies a lens' value from one value to another over 1 second
(.~~) :: Tweenable b => ASetter s s a b -> b -> b -> Animation (s -> s)
(.~~) lns v1 v2 = Animation 1 $ \time -> set lns (tween v1 v2 time)

-- |(.~%) produces an animated modifier that modifies a lens' value from its current value to a new value over 1 second
(.~%) lns v2 = Animation 1 $ \time -> over lns (\v1 -> tween v1 v2 time)

-- |elastic provides an elastic ease onto a linear animation
-- Implementation stolen from https://joshondesign.com/2013/03/01/improvedEasingEquations
elastic = tmap_n innerElastic
  where
  innerElastic t = (2 ** (-10 * t)) * sin((t-p/4)*(2*pi)/p) + 1
  p = 0.3

-- |quint provides a quintic function ease onto a linear animation
quint = tmap_n $ \time -> 1 - ((1 - time) ** 5)

-- |between is just a semantic convenience
-- It finds a value 0.5 of the way between two values, for example.
-- That's also what tween does at second 0.5
-- But I don't want to put tween everywhere, because it'll look like I'm
--   animating things I'm not
-- So I use between for "static" values
between factor v1 v2 = tween v1 v2 factor

