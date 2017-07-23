module Main where

import Lib
import Example

import Control.Monad (forM_)
import Codec.Picture (writePng)
import System.Environment (getArgs)

main :: IO ()
main = do
  let frameRate = 24
  let frames = zip [0..] $ map drawScene $ sampleAt frameRate testAnimation
  args <- getArgs
  case args of
    -- A particular second was requested
    x:_ -> let fr = floor (frameRate * read x) in writePng "picked.png" $ snd $ frames !! fr
    otherwise -> forM_ frames $ \(idx, img) -> writePng ("frame" ++ show idx ++ ".png") img
