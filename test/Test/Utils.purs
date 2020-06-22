module Test.Utils where

import Prelude

import Concur.Core.Types (Widget, affAction, unWidget)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, Canceler(..), makeAff, runAff_)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)

data WidgetOp v a
  = Result a
  | InitialView (Maybe v)
  | UpdateView v

derive instance eqWidgetOp :: (Eq v, Eq a) => Eq (WidgetOp v a)

derive instance genericWidgetOp :: Generic (WidgetOp v a) _

instance showWidgetOp :: (Show v, Show a) => Show (WidgetOp v a) where
  show= genericShow

runWidgetAsAff
  :: forall v a
   . Int
  -> Widget v a
  -> Aff (Array (WidgetOp v a))
runWidgetAsAff timeout widget =
  makeAff \affCb -> do
    end <- Ref.new false
    ops <- Ref.new []
    let doneCb = affCb <<< Right =<< Ref.read ops
    _ <- setTimeout timeout doneCb
    iv <- unWidget widget $ handler doneCb end ops
    Ref.modify_ (_ <> [InitialView iv]) ops
    ifM (Ref.read end) doneCb (Ref.write true end)
    -- Currently we can't cancel a widget
    pure noopCanceler
  where
    noopCanceler =
      Canceler (const $ pure unit)

    handler doneCb end ops = case _ of
      Left v -> do
        Ref.modify_ (_ <> [UpdateView v]) ops
      Right a -> do
        Ref.modify_ (_ <> [Result a]) ops
        ifM (Ref.read end) doneCb (Ref.write true end)


affWidget :: forall v a. Monoid v => Maybe v -> Aff a -> Widget v a
affWidget view aff =
  affAction \cb -> do
    let cont = case _ of
          Left e -> pure unit
          Right a -> cb (Right a)
    runAff_ cont aff
    pure view
