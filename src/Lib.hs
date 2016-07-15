module Lib
    ( someFunc
    ) where

import Data.LinearProgram
import Data.LinearProgram.GLPK

objFun :: LinFunc String Double
objFun = varSum ["cream", "water"]

lp :: LP String Double
lp = execLPM $ do
  setDirection Min
  setObjective objFun
  constrain' "Fat" (linCombination [(0.36, "cream"), (0.0, "water")]) (Equ 0.18)
  constrain' "Total" (varSum ["cream", "water"]) (Equ 1.0)
  varBds "cream" 0 1
  varBds "water" 0 1

someFunc :: IO ()
someFunc = print =<< glpSolveVars simplexDefaults lp
