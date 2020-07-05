module Concur.Core.Types where

import Prelude

import Control.Alternative (class Alternative)
import Control.Bind (ifM)
import Control.Monad.RWS (modify_)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.Plus (class Alt, class Plus, alt, empty)
import Control.ShiftMap (class ShiftMap)
import Data.Array (fold)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Traversable (sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (makeAff)
import Effect.Aff.Compat (mkEffectFn1, runEffectFn1)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Unsafe.Coerce (unsafeCoerce)

-- FAQ: What's stopping the widget from calling the handler again after having returned a value (Right a)?
-- Ans: Discipline.
--
-- * Canceller
--
-- The first argument is completion callback. Its called when the cancel has completed.
-- It just not `Effect Unit` to
--
--   * If the canceller is invoked, it should always callback completion callback once.
--   * The completion callback could be called back syncrounously.
--     If there is nothing to cancel, completion callback should be called immideiatly.
--   * Once the completion callback is called, `(EffectFn1 (Either v a) Unit)' should not yield value anymore.
--   * canceller could be called multiple times, even for terminated widgets.
--     Even for such cases, completion callback should be called.
--
-- * Exception
--
-- Currently doesn't handle nor have a semantics about exception.
-- When thinking about exception, we'll need to consider how cancellation works with exception too.
--
type WithHandler v a =
  (EffectFn1 (Either v a) Unit) ->
  Effect { view :: Maybe v, canceller :: Effect Unit -> Effect Unit }

mapViewWithHandler :: forall v1 v2 a. (v1 -> v2) -> WithHandler v1 a -> WithHandler v2 a
mapViewWithHandler f w1 = \cb -> do
  v <- w1 $ mkEffectFn1 \eval -> case eval of
    Left v -> runEffectFn1 cb (Left (f v))
    Right a -> runEffectFn1 cb (Right a)
  pure $ f <$> v

-- A Widget is an initial view, followed by a series of async views
newtype Widget v a = Widget (WithHandler v a)

unWidget :: forall v a. Widget v a -> WithHandler v a
unWidget (Widget f) = f

unWidgetArray :: forall v a. Array (Widget v a) -> Array (WithHandler v a)
unWidgetArray arr = unsafeCoerce arr

mkWidgetArray :: forall v a. Array (WithHandler v a) -> Array (Widget v a)
mkWidgetArray arr = unsafeCoerce arr

instance functorWidget :: Functor (Widget v) where
  map f (Widget g) = Widget \cb -> g $ mkEffectFn1 \val -> runEffectFn1 cb $ map f val

instance widgetBind :: Bind (Widget v) where
  bind (Widget f) h = Widget \cb ->
    let fing = mkEffectFn1 $ case _ of
          Left v -> runEffectFn1 cb (Left v)
          Right a -> do
            mv <- unWidget (h a) cb
            case mv of
              Nothing -> pure unit
              Just v -> runEffectFn1 cb (Left v)
    in f fing

instance widgetApplicative :: Applicative (Widget v) where
  pure a = Widget \cb -> runEffectFn1 cb (Right a) *> pure Nothing

instance widgetApply :: Apply (Widget v) where
  apply x y = do
    a <- x
    b <- y
    pure (a b)

instance widgetMonad :: Monad (Widget v)

-- derive newtype instance widgetMonadRec :: MonadRec (Widget v)

instance widgetShiftMap :: ShiftMap (Widget v) (Widget v) where
  shiftMap f = f identity

instance widgetMultiAlternative :: (Monoid v) => MultiAlternative (Widget v) where
  orr wss = Widget \cb -> do
    -- Oh the mutation!
    doneRef <- Ref.new false
    viewsRef <- Ref.new [Nothing]
    let
      -- mkCb :: Int -> Either v a -> Effect Unit
      mkCb i = mkEffectFn1 \eval -> case eval of
        Right a -> do
          isDone <- Ref.read doneRef
          when (not isDone) do
            Ref.write true doneRef
            runEffectFn1 cb (Right a)
        Left v -> do
          isDone <- Ref.read doneRef
          when (not isDone) do
            vs <- Ref.read viewsRef
            let mvs' = A.updateAt i (Just v) vs
            case mvs' of
              Nothing -> pure unit
              Just vs' -> do
                Ref.write vs' viewsRef
                case sequence vs' of
                  Nothing -> pure unit
                  Just arr -> runEffectFn1 cb $ Left $ fold arr
    vs <- traverseWithIndex (\i f -> f (mkCb i)) (unWidgetArray wss)
    Ref.write vs viewsRef
    case sequence vs of
      Nothing -> pure Nothing
      Just arr -> pure $ Just $ fold arr


instance widgetSemigroup :: (Monoid v) => Semigroup (Widget v a) where
  append w1 w2 = orr [w1, w2]

instance widgetMonoid :: (Monoid v) => Monoid (Widget v a) where
  mempty = empty

instance widgetAlt :: (Monoid v) => Alt (Widget v) where
  alt = append

instance widgetPlus :: (Monoid v) => Plus (Widget v) where
  empty = Widget \cb -> pure $ Just mempty

instance widgetAlternative :: (Monoid v) => Alternative (Widget v)

-- | Run multiple widgets in parallel until *all* finish, and collect their outputs
-- | Contrast with `orr`
-- TODO: Performance? Don't orr with `empty`.
andd ::
  forall v a.
  Monoid v =>
  Array (Widget v a) ->
  Widget v (Array a)
andd ws = do
  Tuple i e <- foldrWithIndex (\i w r -> alt (map (Tuple i) w) r) empty ws
  let ws' = fromMaybe ws $ A.deleteAt i ws
  if A.length ws' <= 0
    then pure [e]
    else do
      rest <- andd ws'
      pure $ fromMaybe [] $ A.insertAt i e rest


mapView :: forall a v1 v2. (v1 -> v2) -> Widget v1 a -> Widget v2 a
mapView f (Widget w) = Widget (mapViewWithHandler f w)

display :: forall v a. v -> Widget v a
display v = Widget \cb -> pure (Just v)

-- Sync eff
effAction ::
  forall a v.
  Effect a ->
  Widget v a
effAction eff = Widget \cb -> do
  void $ setTimeout 0 do
    a <- eff
    runEffectFn1 cb (Right a)
  pure Nothing

-- Async aff
-- Make sure that the callback is called asyncnourously.
-- Any synchronuse callback should be called before any asyncnourous call are made.
-- There for we are calling setTimeout 0 *before* evaluating *handler*.
affAction ::
  forall a v.
  WithHandler v a ->
  Widget v a
affAction handler = Widget \cb -> do
  stillSync <- Ref.new true
  syncVas <- Ref.new []
  void $ setTimeout 0 do
    traverse_ (runEffectFn1 cb) =<< Ref.read syncVas
  view <- handler $ mkEffectFn1 \va -> do
    ifM (Ref.read stillSync)
      do Ref.modify_ (_ <> [va]) syncVas
      do runEffectFn1 cb va
    pure unit
  Ref.write false stillSync
  pure view

-- Async callback
-- asyncAction
--   :: forall v a
--   .  ((a -> Effect Unit) -> Effect (Effect Unit))
--   -> Widget v a
-- asyncAction handler = affAction (Observer handler)

instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect = effAction

-- instance widgetMonadObserver :: (Monoid v) => MonadObserver (Widget v) where
--   liftObserver = affAction mempty
    -- Widget $ liftF $ WidgetStep $ Right { view: mempty, cont: aff }

mkNodeWidget :: forall v1 v2 a. ((a -> Effect Unit) -> v1 -> v2) -> Widget v1 a -> Widget v2 a
mkNodeWidget h (Widget f) = Widget \cb ->
  mapViewWithHandler (h \a -> runEffectFn1 cb (Right a)) f cb

mkLeafWidget :: forall v a. ((a -> Effect Unit) -> v) -> Widget v a
mkLeafWidget h = Widget \cb -> pure $ Just $ h \a -> runEffectFn1 cb (Right a)
