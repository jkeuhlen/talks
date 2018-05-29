{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Arrows where

import Data.Extensible.Sum
import Data.Extensible.Sum2
import Data.Extensible.Product
import Data.Text(Text)
import Control.Arrow
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Category as C
import Control.Monad
import Control.Monad.IO.Class


import Debug.Trace
-- Free Arrow
data FreeA eff a b where
    Pure :: (a -> b) -> FreeA eff a b
    Effect :: eff a b -> FreeA eff a b
    Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
    Par :: FreeA eff a1 b1 -> FreeA eff a2 b2 -> FreeA eff (a1, a2) (b1, b2)
    -- Apply -- | Arrow apply 
    -- FanIn -- | Arrow Choice
    -- Spl   -- | Arrow Choice

instance C.Category (FreeA eff) where
    id = Pure id
    (.) = flip Seq

instance Arrow (FreeA eff) where
    arr = Pure
    first f = Par f C.id
    second f = Par C.id f
    (***) = Par

compileA :: forall eff arr a0 b0. (Arrow arr) => (forall a b. eff a b -> arr a b) -> FreeA eff a0 b0 -> arr a0 b0
compileA exec = go
  where
    go :: forall a b . (Arrow arr) => FreeA eff a b -> arr a b
    go freeA = case freeA of
        Pure f -> arr f
        Seq f1 f2 -> go f2 C.. go f1
        Par f1 f2 -> go f1 *** go f2
        Effect eff -> exec eff

evalKleisliA :: forall m a b . 
  (
    Monad m
  ) => FreeA (Kleisli m) a b 
    -> Kleisli m a b
evalKleisliA = go
  where
    go :: forall m a b . (Monad m) => FreeA (Kleisli m) a b -> Kleisli m a b
    go freeA = case freeA of
        Pure f -> Kleisli $ return . f
        Effect eff -> eff
        Seq f1 f2 -> go f2 C.. go f1
        Par f1 f2 -> go f1 *** go f2


liftK :: Monad m => (b -> m c) -> FreeA (Kleisli m) b c
liftK eff = Effect (Kleisli $ \x -> eff x)

-- Free effects
data PrintX a b where 
  Screen :: PrintX Text () 

cmplPrintX :: (MonadIO m) => PrintX a b -> FreeA (Kleisli m) a b 
cmplPrintX Screen = liftK (\x -> liftIO $ T.putStrLn x)

cmplPrintXToFile :: (MonadIO m) => PrintX a b -> FreeA (Kleisli m) a b 
cmplPrintXToFile Screen = liftK (\x -> liftIO $ T.writeFile "output.txt" x)

data Print2X a b where 
  Screen2 :: Print2X String ()

cmplPrint2X :: (MonadIO m) => Print2X a b -> FreeA (Kleisli m) a b 
cmplPrint2X Screen2 = liftK (\x -> liftIO $ putStrLn x)

cmplPrint2XToFile :: (MonadIO m) => Print2X a b -> FreeA (Kleisli m) a b 
cmplPrint2XToFile Screen2 = liftK (\x -> liftIO $ writeFile "output.txt" x)  


-- Sum Class information/helper functions 
lftEff :: (eff :>+: f) 
  => FreeA f a b 
  -> FreeA eff a b
lftEff = fmapEff lft2

lftE :: (eff :>+: f)
  => f a b -> FreeA eff a b
lftE = lftEff . Effect

fmapEff :: forall b c eff1 eff2 . (forall bb cc . eff1 bb cc -> eff2 bb cc)
  -> FreeA eff1 b c -> FreeA eff2 b c
fmapEff fxn = go
  where
    go :: forall b c . FreeA eff1 b c -> FreeA eff2 b c 
    go (Effect eff) = Effect $ fxn eff 
    go (Pure x) = Pure x 
    go (Seq f1 f2) = go f2 C.. go f1
    go (Par f1 f2) = go f1 *** go f2    

lftEffA :: forall b c eff1 eff2 . (forall bb cc . eff1 bb cc -> FreeA eff2 bb cc)
  -> FreeA eff1 b c -> FreeA eff2 b c
lftEffA fxn = go
  where
    go :: forall b c . FreeA eff1 b c -> FreeA eff2 b c 
    go (Effect eff) = fxn eff 
    go (Pure x) = Pure x 
    go (Seq f1 f2) = go f2 C.. go f1
    go (Par f1 f2) = go f1 *** go f2


(<#>) ::
     (forall a' b' . f a' b' -> FreeA h a' b')
  -> (forall a' b' . g a' b' -> FreeA h a' b' )
  -> (f :+: g) a b
  -> FreeA h a b
(<#>) f2hA _ (InL x) = f2hA x
(<#>) _ g2hA (InR x) = g2hA x

(#>>) :: 
     (forall a' b' . f a' b' -> FreeA g a' b')
  -> (forall a' b' . g a' b' -> FreeA h a' b' )
  -> f a b
  -> FreeA h a b
(#>>) f2gA g2hA x = lftEffA g2hA $ f2gA x

-- | Example arrows 

extensibleArrow :: (eff :>+: PrintX, eff :>+: Print2X) => FreeA eff Text ()
extensibleArrow = proc x -> do 
  print1 -< x 
  print2 -< T.unpack x 
  Pure id -< ()

print1 :: (eff :>+: PrintX) => FreeA eff Text ()
print1 = lftE Screen 

print2 :: (eff :>+: Print2X) => FreeA eff String ()
print2 = lftE Screen2