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
main_for i = print -- 
  $ foldr ($) (speed (run 16))
  $ replicate i
  $ sometimes (# speed 0)

main_for_1 i = print -- works
  $ foldr ($) (run 16)
  $ replicate i
  $ sometimes (const 0)
