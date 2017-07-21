module Main where

import Lib
import Example

import Control.Monad (forM_)
import Codec.Picture (writePng)

main :: IO ()
main = forM_ (zip [0..] $ map drawScene $ sampleAt 24 testAnimation) $ \(idx, img) -> writePng ("frame" ++ show idx ++ ".png") img
