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
    "1" -> runKleisli (evalKleisliA $ compileA cmplPrintX $ Effect Print >>> Pure (const "foo") >>> Effect Print) ("'ello World!" :: Text)
    "2" -> runKleisli (evalKleisliA $ compileA (cmplPrintX <#> cmplStoreXToFile) extensibleArrow) ("Extensible Arrow" :: Text)
    _   -> runKleisli (evalKleisliA $ compileA cmplPrintXToFile $ Effect Print) ("'ello File!" :: Text)




   