module Lib
    ( someFunc
    ) where

import Data.LinearProgram
import Data.LinearProgram.GLPK

data Ingredient = Ingredient
  { name :: String
  , fat :: Double
  , sweetness :: Double
  , solids :: Double
  }
  deriving Eq

instance Show Ingredient where
  show = name

instance Ord Ingredient where
  compare a b = name a `compare` name b

cream = Ingredient
  { name = "cream"
  , fat = 0.36
  , sweetness = 0
  , solids = 0.45
  }

water = Ingredient
  { name = "water"
  , fat = 0
  , sweetness = 0
  , solids = 0
  }

ingredients = [cream, water]

objFun :: LinFunc Ingredient Double
objFun = varSum ingredients

lp :: LP Ingredient Double
lp = execLPM $ do
  setDirection Min
  setObjective objFun
  constrain' "Fat" (linCombination [(0.36, cream), (0.0, water)]) (Equ 0.18)
  constrain' "Solids" (linCombination [(0.45, cream), (0.0, water)]) (Bound 0.2 1.0)
  constrain' "Total" (varSum ingredients) (Equ 1.0)
  varBds cream 0 1
  varBds water 0 1

someFunc :: IO ()
someFunc = print =<< glpSolveVars simplexDefaults lp
