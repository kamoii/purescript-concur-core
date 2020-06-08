module Concur.Core.Event where

import Prelude

import Control.Alt ((<|>))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence_)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref

-- TODO: Generalise monad to m
-- TODO: This is basically ContT, apart from the canceller
-- Returns a canceller
newtype Observer a = Observer ((a -> Effect Unit) -> Effect (Effect Unit))
-- derive instance observeFunctor :: Functor Observer
instance observeFunctor :: Functor Observer where
  map f (Observer g) = Observer \cb -> g \a -> cb $ f a

-- Chain an effect onto an observer
effMap :: forall a b. Observer a -> (a -> Effect b) -> Observer b
effMap (Observer g) f = Observer \cb -> g \a -> cb =<< f a

-- TODO: Monadic chaining for observer

-- Mutable な refenence を使う以外に思い浮ばない...
-- 注意するケースとして Observer が実際に非同期ではなく、則継続が呼ばれる場合
-- そもそも canceller の semantics を明確にしておく必要がある
-- 一度処理が終わり継続が呼ばれた後は効果をなさない？
instance observeApply :: Apply Observer where
  -- apply :: forall a b. f (a -> b) -> f a -> f b
  apply (Observer f) (Observer a) =
    Observer \cb -> do
      ref <- Ref.new Nothing
      fc <- f \vf -> do
        ac <- a (cb <<< vf)
        Ref.write (Just ac) ref
      -- もし f の実態が同期的な処理の場合、既に Just ac が設定されている。
      -- そのため Nothing の場合のみ fc を設定する
      Ref.modify_ (_ <|> Just fc) ref
      pure $ maybe (pure unit) identity =<< Ref.read ref

instance observerApplicative :: Applicative Observer where
  pure a = Observer \cb -> (cb a) *> pure (pure unit)

instance observerBind :: Bind Observer where
  bind (Observer a) f =
    Observer \cb -> do
      ref <- Ref.new Nothing
      ac <- a \va -> do
        let Observer b = f va
        bc <- b cb
        Ref.write (Just bc) ref
      -- もし f の実態が同期的な処理の場合、既に Just ac が設定されている。
      -- そのため Nothing の場合のみ fc を設定する
      Ref.modify_ (_ <|> Just ac) ref
      pure $ maybe (pure unit) identity =<< Ref.read ref

instance observerMonad :: Monad Observer

instance observerMonadEffect :: MonadEffect Observer where
  liftEffect eff =
    Observer \cb -> do
      cb =<< eff
      pure $ pure unit

observe :: forall a. Observer a -> (a -> Effect Unit) -> Effect (Effect Unit)
observe (Observer f) = f

never :: forall a. Observer a
never = Observer \_ -> pure (pure unit)

dont :: forall a. Pusher a
dont a = pure unit

-- Push data
type Pusher a = a -> Effect Unit


-----------------------------------------------------------------------
----------------------- LOOK FOR BUGS HERE ----------------------------
--- WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING ---
-- BELOW ARE THE ONLY MUTABLE, STATEFUL BITS IN THE ENTIRE FRAMEWORK --
-----------------------------------------------------------------------

parIndex :: forall a. Array (Observer a) -> Observer ({i::Int, val::a})
parIndex = par' \i val -> {i, val}

par :: forall a. Array (Observer a) -> Observer a
par = par' \_ val -> val

par' :: forall a b. (Int -> a -> b) -> Array (Observer a) -> Observer b
par' g os = Observer \cb -> do
  ref <- Ref.new []
  cs <- traverseWithIndex (\i (Observer f) -> f \a -> do
          cs <- Ref.read ref
          Ref.write [] ref
          traverseWithIndex_ (\j -> when (j /= i)) cs
          cb $ g i a
    ) os
  Ref.write cs ref
  pure do
    sequence_ cs
    Ref.write [] ref

mkObserver :: forall a. Effect { push :: Pusher a, subscribe :: Observer a }
mkObserver = do
  ref <- Ref.new Nothing
  let push a = maybe (pure unit) (_ $ a) =<< Ref.read ref
  let subscribe = Observer \cb -> do
        Ref.write (Just cb) ref
        pure do Ref.write Nothing ref
  pure { push, subscribe}
