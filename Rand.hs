{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

import System.Environment
import System.IO
import Sound.Tidal.Context

main = do
  hSetBuffering stdout NoBuffering
  getArgs >>= \ case
    [] -> main_for 10
    [s] -> main_for  $ read s

-- see https://github.com/tidalcycles/Tidal/issues/636

main_for :: Int -> IO ()
main_for = main_for_2

-- see https://github.com/tidalcycles/Tidal/issues/636#issuecomment-618433347
main_for_2 i = print
  $ foldr ($) (speed (run 16))
  $ replicate i
  $ _sometimesBy (1/2) (# speed 0)

_sometimesBy  :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_sometimesBy x f p = overlay (_degradeBy x p) (_unDegradeBy x $ f p)

main_for_1 i = print 
  $ foldr ($) (run 16)
  $ replicate i
  $ sometimes (const 0)

main_for_0 i = print 
  $ foldr ($) (speed (run 16))
  $ replicate i
  $ sometimes (# speed 0) 
