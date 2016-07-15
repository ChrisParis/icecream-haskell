{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import Data.Map.Lazy
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

instance Show Property where
  show (Property name _ _) = name

instance Ord Property where
  compare (Property a _ _) (Property b _ _) = a `compare` b

instance Eq Property where
  (==) (Property a _ _) (Property b _ _) = a == b

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

sugar = Ingredient
  { name = "sugar"
  , fat = 0
  , sweetness = 1
  , solids = 1
  , bounds = Bound 0 1
  }

chocolate = Ingredient
  { name = "chocolate"
  , fat = 0.36
  , sweetness = 0.19
  , solids = 1
  , bounds = Equ 0.05
  }

ingredients = [cream, water, sugar, chocolate]

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

propertyTotal :: Map Ingredient Double -> Property -> Double
propertyTotal ingredientAmounts (Property _ get _) =
  foldrWithKey f 0 ingredientAmounts
  where
    f ingredient iFrac total = total + iFrac * (get ingredient)

propertyTotals :: Map Ingredient Double -> [Property] -> Map Property Double
propertyTotals ingredientAmounts properties =
  fromList $ zip properties $ fmap (propertyTotal ingredientAmounts) properties

objFun :: LinFunc Ingredient Double
objFun = varSum ingredients

lp :: LP Ingredient Double
lp = execLPM $ do
  setDirection Min
  setObjective objFun
  buildConstraints properties ingredients
  constrain' "Total" (varSum ingredients) (Equ 1.0)

someFunc :: IO ()
someFunc = do
  (code, vars) <- glpSolveVars simplexDefaults lp
  print (code, vars)
  case vars of
    Nothing -> return ()
    Just (_, ingredientAmounts) -> do
      putStr "Formula totals: "
      print $ propertyTotals ingredientAmounts properties
