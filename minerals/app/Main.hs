module Main where

import Pauling
import Type
import Vis
import Linear.V3
import CFParser
import Basic (testModel)
import System.Environment (getArgs)

mineralWindow = defaultOpts{
    optWindowName = "Mineral Simulator",
    optWindowSize = Just (640,360),
    optWindowPosition = Just (500,100),
    optBackgroundColor = Just white
}

main :: IO ()
main = do
    (formula:_) <- getArgs
    let elems = runParseCF formula
    -- let elemsStr = (Text3d (show elems) (V3 1 2 0) TimesRoman10 red)
    -- let testModels = VisObjects $ elemsStr:testModel
    display mineralWindow testModel