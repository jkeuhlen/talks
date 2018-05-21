{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arrows where

import Data.Extensible.Sum
import Data.Extensible.Sum2
import Data.Extensible.Product
import Data.Text(Text)
import Control.Arrow
import qualified Control.Category as C
import Control.Monad
import Control.Monad.IO.Class

-- Free Arrow
data FreeA eff a b where
    Pure :: (a -> b) -> FreeA eff a b
    Effect :: eff a b -> FreeA eff a b
    Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
    Par :: FreeA eff a1 b1 -> FreeA eff a2 b2 -> FreeA eff (a1, a2) (b1, b2)
    -- Apply -- | Arrow apply 
    -- FanIn -- | Arrow Choice
    -- Spl   -- | Arrow Choice

effect :: eff a b -> FreeA eff a b
effect = Effect

instance C.Category (FreeA eff) where
    id = Pure id
    (.) = flip Seq

instance Arrow (FreeA eff) where
    arr = Pure
    first f = Par f C.id
    second f = Par C.id f
    (***) = Par

evalA :: forall eff arr a0 b0. (Arrow arr) => (forall a b. eff a b -> arr a b) -> FreeA eff a0 b0 -> arr a0 b0
evalA exec = go
  where
    go :: forall a b . (Arrow arr) => FreeA eff a b -> arr a b
    go freeA = case freeA of
        Pure f -> arr f
        Seq f1 f2 -> go f2 C.. go f1
        Par f1 f2 -> go f1 *** go f2
        Effect eff -> exec eff

-- evalA :: forall m a b . 
--   (
--     Monad m
--   ) => FreeA (Kleisli m) a b 
--     -> Kleisli m a b
-- evalA = go
--   where
--     go :: (Monad m) => FreeA (Kleisli m) a b -> Kleisli m a b
--     go freeA = case freeA of
--         Pure f -> Kleisli $ return . f
--         Effect eff -> eff
--         Seq f1 f2 -> go f2 C.. go f1
--         Par f1 f2 -> go f1 *** go f2


liftK :: Monad m => (b -> m c) -> FreeA (Kleisli m) b c
liftK eff = effect (Kleisli $ \x -> eff x)

-- Free effect
data PrintX a b where 
  Screen :: PrintX Text () 

cmplPrintX :: (MonadIO m) => PrintX a b -> FreeA (Kleisli m) a b 
cmplPrintX Screen = liftK $ const $ liftIO $ putStrLn "Foo!"

