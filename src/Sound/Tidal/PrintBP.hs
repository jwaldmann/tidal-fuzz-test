{-# language ExplicitForAll #-}
{-# language TypeApplications #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language DeriveGeneric #-}

module Sound.Tidal.PrintBP where

import Prelude hiding (print)

import Sound.Tidal.ParseBP

import Test.LeanCheck
import Test.LeanCheck.Generic
import GHC.Generics
import Data.Ratio

deriving instance Eq a => Eq (TPat a)
deriving instance Generic (TPat a)

instance Listable a => Listable (TPat a) where
  tiers = genericTiers

t1 = checkFor 1000 (p1 @Bool) 

p1
  :: forall a
  . (Parseable a, Eq a,Printable a, Show a)
  => TPat a
  -> Bool
p1 p = case parseTPat (print p) of
  Left e  -> error $ unlines
    [ "could not parse", show p,  print p ]
  Right q -> normalize p ==  normalize q ||
    error ( unlines
      ["different values", show p, print p, show q] )

class Printable a where print :: a -> String
instance Printable Bool where
  print False = "f" ; print True = "t"
instance Printable Integer where print = show
instance Printable a => Printable (Ratio a) where
  print q = print (numerator q) <> "%" <> print (denominator q)

-- | remove source span info in atoms, and some other cleanup
normalize :: TPat a -> TPat a
normalize (TPat_Seq [p]) = normalize p
normalize (TPat_Atom _ x) = TPat_Atom Nothing x
normalize p = p

instance Printable a => Printable (TPat a) where
 print = \ case
  --    TPat_Atom (Maybe ((Int, Int), (Int, Int))) a
  TPat_Atom m x -> print x
  TPat_Fast t p -> print p <> "*" <> print t 
  --  | TPat_Slow (TPat Time) (TPat a)
  --  | TPat_DegradeBy Int Double (TPat a)
  --  | TPat_CycleChoose Int [TPat a]
  --  | TPat_Euclid (TPat Int) (TPat Int) (TPat Int) (TPat a)
  --  | TPat_Stack [TPat a]
  --  | TPat_Polyrhythm (Maybe (TPat Rational)) [TPat a]
  --  | TPat_Seq [TPat a]
  TPat_Silence -> "~"
  TPat_Foot -> "."
  --  | TPat_Elongate Rational (TPat a)
  --  | TPat_Repeat Int (TPat a)
  --  | TPat_EnumFromTo (TPat a) (TPat a)
