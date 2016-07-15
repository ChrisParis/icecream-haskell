{-# LANGUAGE FlexibleContexts #-}

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
  , bounds :: Bounds Double
  }
  deriving Eq

instance Show Ingredient where
  show = name

instance Ord Ingredient where
  compare a b = name a `compare` name b

data Property = Property String (Ingredient -> Double) (Bounds Double)

cream = Ingredient
  { name = "cream"
  , fat = 0.36
  , sweetness = 0
  , solids = 0.45
  , bounds = Bound 0 1
  }

water = Ingredient
  { name = "water"
  , fat = 0
  , sweetness = 0
  , solids = 0
  , bounds = Bound 0 1
  }

ingredients = [cream, water]

properties = [ Property "Fat" fat (Equ 0.18)
             , Property "Sweetness" sweetness (Equ 0.2)
             , Property "Solids" solids (Bound 0.2 1.0)
             ]

buildConstraints properties ingredients = do
  let buildConstraint (Property label get bounds) =
        constrain' label (linCombination (zip (fmap get ingredients) ingredients)) bounds
      buildVarBounds ingredient = setVarBounds ingredient $ bounds ingredient
  mapM_ buildConstraint properties
  mapM_ buildVarBounds ingredients

objFun :: LinFunc Ingredient Double
objFun = varSum ingredients

lp :: LP Ingredient Double
lp = execLPM $ do
  setDirection Min
  setObjective objFun
  buildConstraints properties ingredients
  constrain' "Total" (varSum ingredients) (Equ 1.0)

someFunc :: IO ()
someFunc = print =<< glpSolveVars simplexDefaults lp
