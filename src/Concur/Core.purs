module Concur.Core where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Aff (Aff, never)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, hoistFree, resume, liftF)
import Control.Monad.IO (IO)
import Control.Monad.IOSync (IOSync)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (class Alt, class Plus, alt, (<|>), empty)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Monoid (class Monoid, mempty)

newtype WidgetStep v a = WidgetStep (IOSync
  { view :: v
  , cont :: IO a
  })

instance functorWidgetStep :: Functor (WidgetStep v) where
  map f (WidgetStep w) = WidgetStep (map mod w)
    where mod ws = ws { cont = map f ws.cont }

newtype Widget v a = Widget (Free (WidgetStep v) a)

unWidget :: forall v a. Widget v a -> Free (WidgetStep v) a
unWidget (Widget w) = w

instance widgetFunctor :: Functor (Widget v) where
  map k (Widget w) = Widget (map k w)

instance widgetBind :: Bind (Widget v) where
  bind (Widget w) f = Widget (bind w (unWidget <<< f))

instance widgetApplicative :: Applicative (Widget v) where
  pure = Widget <<< pure

instance widgetApply :: Apply (Widget v) where
  apply = ap

instance widgetMonad :: Monad (Widget v)

instance widgetSemigroup :: Semigroup v => Semigroup (Widget v a) where
  append (Widget w1) (Widget w2) = Widget (appendFree w1 w2)
    where
      appendFree w1 w2 =
        case resume w1 of
          Right a1 -> pure a1
          Left ws1 -> case resume w2 of
            Right a2 -> pure a2
            Left ws2 -> join (liftF (appendWidgetStep ws1 ws2))
      appendWidgetStep (WidgetStep ws1) (WidgetStep ws2) = WidgetStep (appendWidgetStepInner <$> ws1 <*> ws2)
      appendWidgetStepInner ws1' ws2' =
        { view : ws1'.view <> ws2'.view
        , cont : sequential (alt (parallel ws1'.cont) (parallel ws2'.cont))
        }

instance widgetAlt :: Semigroup v => Alt (Widget v) where
  alt = append

instance widgetPlus :: Monoid v => Plus (Widget v) where
  empty = display mempty

instance widgetAlternative :: Monoid v => Alternative (Widget v)

mapView :: forall a v. (v -> v) -> Widget v a -> Widget v a
mapView f (Widget w) = Widget (hoistFree (mapViewStep f) w)

mapViewStep :: forall v1 v2 a. (v1 -> v2) -> WidgetStep v1 a -> WidgetStep v2 a
mapViewStep f (WidgetStep ws) = WidgetStep (map mod ws)
  where mod ws' = ws' { view = f ws'.view }

display :: forall a v. v -> Widget v a
display v = Widget (liftF (WidgetStep (pure { view: v, cont: liftAff never })))

orr :: forall m a. Plus m => Array (m a) -> m a
orr = foldl (<|>) empty

-- Unfortunately 'affect' is also the verb form of 'effect'
-- So we can't use `affect` and `effect` for these

-- Sync but Non blocking eff
effAction :: forall a v eff. v -> Eff eff a -> Widget v a
effAction v eff = Widget (liftF (WidgetStep (pure { view: v, cont: liftEff eff })))

-- Sync and blocking eff
-- WARNING: UNSAFE: This will block the UI rendering
unsafeBlockingEffAction :: forall a v. v -> IOSync a -> Widget v a
unsafeBlockingEffAction v eff = Widget (liftF (WidgetStep (eff >>= \a -> pure { view: v, cont: pure a })))

-- Async aff
affAction :: forall a v eff. v -> Aff eff a -> Widget v a
affAction v aff = Widget (liftF (WidgetStep (pure { view: v, cont: liftAff aff })))

instance widgetMonadEff :: Monoid v => MonadEff eff (Widget v) where
  liftEff = effAction mempty

instance widgetMonadAff :: Monoid v => MonadAff eff (Widget v) where
  liftAff = affAction mempty
