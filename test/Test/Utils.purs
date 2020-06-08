module Test.Utils where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Event (Observer(..))
import Concur.Core.Types (WidgetStep(..), unWidget)
import Control.Monad.Free (runFreeM)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer (clearTimeout, setTimeout)

-- Evalutates Widget to Aff
-- Be carefull that never ending Widget will convert to never ending Aff.
runWidgetAsAff :: forall v a. Widget v a -> Aff { result :: a, views :: Array v }
runWidgetAsAff widget = do
  Tuple result views <- runWriterT $ runFreeM interpret (unWidget widget)
  pure { result, views }
  where
    interpret (WidgetStepEff eff) =
      liftEffect eff

    interpret (WidgetStepView rec) = do
      tell $ singleton rec.view
      liftAff $ observerToAff rec.cont

-- Converts an Observer to an Aff.
-- Observer can't return an Error, so we always wrap with Right.
observerToAff :: forall a. Observer a -> Aff a
observerToAff (Observer ob) =
  makeAff \cont -> do
    obsCanceller <- ob (cont <<< Right)
    affCanceller <- pure $ wrap $ const $ liftEffect obsCanceller
    pure affCanceller

delayObserver :: Milliseconds -> Observer Unit
delayObserver (Milliseconds msec) =
  Observer \cont -> do
    timeoutId <- setTimeout (round msec) (cont unit)
    pure $ clearTimeout timeoutId

