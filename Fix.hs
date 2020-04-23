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

-- see https://github.com/tidalcycles/Tidal/issues/564

main_for :: Int -> IO ()
main_for i = print -- 
  $ foldr ($) (speed (run 16))
  $ replicate i
  $ fix (# speed 0) (speed 1)


