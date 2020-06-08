module Test.WidgetSpec where

import Prelude

import Concur.Core.Event (effMap, never)
import Concur.Core.Types (affAction)
import Control.MultiAlternative (orr)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Utils (delayObserver, runWidgetAsAff)

widgetSpec :: Spec Unit
widgetSpec =
  describe "Widget" do
    describe "orr" do
      it "should cancel running effects when the widget returns a value" do
        ref <- liftEffect (Ref.new "")
        _ <- runWidgetAsAff $ orr
          [ affAction "a" $ delayObserver (Milliseconds 100.0) `effMap` const (Ref.write "a" ref)
          , affAction "b" $ delayObserver (Milliseconds 150.0) `effMap` const (Ref.write "b" ref)
          ]
        liftEffect (Ref.read ref) `shouldReturn` "a"
        delay (Milliseconds 100.0)
        liftEffect (Ref.read ref) `shouldReturn` "a"

      it "should start all the widgets only once" do
        ref <- liftEffect (Ref.new 0)
        { result, views } <- runWidgetAsAff $ orr
          [ do
               affAction "a0" $ delayObserver (Milliseconds 100.0)
               affAction "a1" $ delayObserver (Milliseconds 100.0)
               pure "a"
          , affAction "b" do
               liftEffect $ Ref.modify_ (_ + 1) ref
               never
          ]
        result `shouldEqual` "a"
        views `shouldEqual` [ "a0b", "a1b" ]
        liftEffect (Ref.read ref) `shouldReturn` 1
