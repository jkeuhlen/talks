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
    Par :: FreeA eff a₁ b₁ -> FreeA eff a₂ b₂ -> FreeA eff (a₁, a₂) (b₁, b₂)
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
    first f = Par f id
    second f = Par id f
    (***) = Par

evalA :: forall eff arr a₀ b₀. (Arrow arr) => (forall a b. eff a b -> arr a b) -> FreeA eff a₀ b₀ -> arr a₀ b₀
evalA exec = go
  where
    go :: forall a b. FreeA eff a b -> arr a b
    go freeA = case freeA of
        Pure f -> arr f
        Seq f₁ f₂ -> go f₂ . go f₁
        Par f₁ f₂ -> go f₁ *** go f₂
        Effect eff -> exec eff

liftK :: Monad m => (b -> m c) -> FreeA (Kleisli m) b c
liftK eff = effect (Kleisli $ \x -> eff x)

-- Free effect
data PrintX a b where 
  Screen :: PrintX Text () 

cmplPrintX :: (MonadIO m) => PrintX a b -> FreeA (Kleisli m) a b 
cmplPrintX Screen = liftK $ const $ liftIO $ putStrLn "Foo!"

