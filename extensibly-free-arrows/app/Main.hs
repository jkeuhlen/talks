{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Arrows
import Control.Arrow 
import Data.Text (Text)
main :: IO ()
main = do 
  i <- getLine 
  case i of 
    "1" -> runKleisli (evalKleisliA $ evalA cmplPrintX $ effect Screen >>> Pure (const "foo") >>> effect Screen) ("'ello World!" :: Text)
    _   -> runKleisli (evalKleisliA $ evalA cmplPrintXToFile $ effect Screen) ("'ello File!" :: Text)



extensibleArrow :: (eff :>+: PrintX, eff :>+: Print2X) => FreeA eff Text b
extensibleArrow = proc x -> do 
   