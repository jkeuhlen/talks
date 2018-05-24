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
    "1" -> runKleisli (evalKleisliA $ evalA cmplPrintX $ effect Screen >>> Pure (const "foo") >>> effect Screen) ("'ello World!" :: Text)
    "2" -> runKleisli (evalKleisliA $ evalA (cmplPrintX <#> cmplPrint2XToFile) extensibleArrow) ("Extensible Arrow" :: Text)
    _   -> runKleisli (evalKleisliA $ evalA cmplPrintXToFile $ effect Screen) ("'ello File!" :: Text)




   