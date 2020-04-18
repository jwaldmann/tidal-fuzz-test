-- | fuzz-testing tidalcycle's mini-language parser
-- and evaluator: we throw some random strings at it
-- and check that it never throws an exception, or hangs.
-- Next step: strings are not completely random.
-- We start from some known good inputs,
-- and then modify and re-combine them.

{-# language PatternSignatures #-}

import qualified Sound.Tidal.Context as C

import System.Random
import Control.Exception
import Control.DeepSeq (force)
import Data.String (fromString)
import Control.Monad (replicateM,forever,void)
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)

main = do
  forever $ do
    
    n <- randomRIO (1, 10)
    let char = toEnum <$> randomRIO (0, 127) -- FIXME
    s <- replicateM n char

    (do -- error "wat"
        evaluate $ force
         $ C.queryArc (fromString s :: C.Pattern C.ControlMap)
         $ C.Arc 0 10
        hPutStr stderr "." 
     ) `catches`
      [ Handler $ \ (e :: C.TidalParseError) -> do
        -- parse errors are tolerated
        hPutStr stderr "-"
      , Handler $ \ (e :: ErrorCall) ->    do
        hPutStrLn stderr $ unlines
          [ "ErrorCall"
          , show s
          , show $ map fromEnum s
          , show e
          ]
        hFlush stderr
        error "huh"
      , Handler $ \ (e :: SomeException ) -> do
        hPutStrLn stderr $ unlines
          [ "SomeException"
          , show s
          , show $ map fromEnum s
          , show e
          ]
        hFlush stderr
        error "huh"
      ]
