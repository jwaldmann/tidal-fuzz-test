{-# language TypeApplications #-}

import Sound.Tidal.PrintBP
import Test.LeanCheck

main = checkFor 1000 (p1 @Bool) 
