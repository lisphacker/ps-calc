module Calc.Key where

import Prelude
import Data.Eq
import Data.Ord
import Data.Ordering (invert)

data Key = Number Int
         | Decimal
         | Add
         | Sub
         | Mul
         | Div
         | Equals

derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key

instance showKey :: Show Key where
  show (Number n) = show n
  show Decimal    = "."
  show Add        = "+"
  show Sub        = "-"
  show Mul        = "*"
  show Div        = "/"
  show Equals     = "="
