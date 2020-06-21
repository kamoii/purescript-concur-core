module Test.WidgetSpec (widgetSpec) where

import Prelude

import Concur.Core.Types (affAction, display)
import Control.MultiAlternative (orr)
import Data.Maybe (Maybe(..))
import Effect.Aff (delay, never)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Utils (WidgetOp(..), runWidgetAsAff)

widgetSpec :: Spec Unit
widgetSpec =
  describe "Widget" do
    displaySpec
    liftEfectfSpec

displaySpec :: Spec Unit
displaySpec =
  describe "display" do
    it "should display initial view" do
      ops <- runWidgetAsAff 100 do
        display "foo" $> unit
      ops `shouldEqual`
        [ InitialView (Just "foo") ]

-- TODO: these test chould be wrong.
liftEfectfSpec :: Spec Unit
liftEfectfSpec = do
  describe "liftEffect" do
    it "can make a widget that do nothing" do
      ops <- runWidgetAsAff 100 do
        liftEffect $ pure unit
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView Nothing
        , Result unit
        ]

    it "shouldn't mess the initial view and updat views order" do
      ops <- runWidgetAsAff 100 do
        liftEffect $ pure unit
        display "foo"
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView Nothing
        , UpdateView "foo"
        ]


    -- describe "orr" do
    --   it "should cancel running effects when the widget returns a value" do
    --     ref <- liftEffect $ Ref.new ""
    --     { views } <- runWidgetAsAff $ orr
    --       [ affAction "a" do
    --            delay (Milliseconds 100.0)
    --            liftEffect $ Ref.write "a" ref
    --       , affAction "b" do
    --            delay (Milliseconds 150.0)
    --            liftEffect $ Ref.write "b" ref
    --       ]
    --     views `shouldEqual` [ "ab" ]
    --     liftEffect (Ref.read ref) `shouldReturn` "a"
    --     delay (Milliseconds 100.0)
    --     liftEffect (Ref.read ref) `shouldReturn` "a"

    --   it "should start all the widgets only once" do
    --     ref <- liftEffect (Ref.new 0)
    --     { result, views } <- runWidgetAsAff $ orr
    --       [ do
    --            affAction "a0" $ delay (Milliseconds 100.0)
    --            affAction "a1" $ delay (Milliseconds 100.0)
    --            pure "a"
    --       , affAction "b" do
    --            liftEffect $ Ref.modify_ (_ + 1) ref
    --            never
    --       ]
    --     result `shouldEqual` "a"
    --     views `shouldEqual` [ "a0b", "a1b" ]
    --     liftEffect (Ref.read ref) `shouldReturn` 1
