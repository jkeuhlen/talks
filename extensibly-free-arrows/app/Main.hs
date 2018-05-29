{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Arrows
import Control.Arrow 
import Data.Text (Text)

main :: IO ()
main = do 
  putStrLn "Pick a way to run this program [1|2|*]"
  i <- getLine 
  case i of 
    "1" -> runKleisli (evalKleisliA $ compileA cmplPrintX $ Effect Screen >>> Pure (const "foo") >>> Effect Screen) ("'ello World!" :: Text)
    "2" -> runKleisli (evalKleisliA $ compileA (cmplPrintX <#> cmplPrint2XToFile) extensibleArrow) ("Extensible Arrow" :: Text)
    _   -> runKleisli (evalKleisliA $ compileA cmplPrintXToFile $ Effect Screen) ("'ello File!" :: Text)




   