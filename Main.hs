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
import Control.Monad (replicateM,forever,void,when)
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)
import qualified Data.Map as M

main = do
  let go tick errs = do
        when (0 == mod tick 1000000)
          $ hPutStr stderr $ show tick <> " "

        n <- randomRIO (1, 100)
        let char = toEnum <$> randomRIO (0, 127) -- FIXME
        s <- replicateM n char

        merr <- (do 
            evaluate $ force
              $ C.queryArc (fromString s :: C.Pattern C.ControlMap)
              $ C.Arc 0 10
            -- hPutStr stderr "."
            return Nothing
          ) `catches`
          [ Handler $ \ (e :: C.TidalParseError) -> do
              -- hPutStr stderr "p"
              return Nothing
          , Handler $ \ (e :: SomeException ) -> do
              -- hPutStr stderr "e"
              return $ Just e
          ]
        errs <- case merr of
          Nothing -> return errs
          Just err -> case M.lookup (show err) errs of
            Nothing -> do
              hPutStrLn stderr $ unlines
                [ "xxx new error: " <> show err
                , "for input"
                , show s
                , show $ map fromEnum s
                ]
              return $ M.insert (show err) s errs
            Just t | length s < length t -> do
              hPutStrLn stderr $ unlines
                [ "xxx old error: " <> show err
                , "new (shorter) input"
                , show s
                , show $ map fromEnum s
                ]
              return $ M.insert (show err) s errs
            _ -> do
              return errs
        go (succ tick) errs
  go (0 :: Int) M.empty
