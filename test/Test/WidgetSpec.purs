module Test.WidgetSpec (widgetSpec) where

import Prelude

import Concur.Core.Types (affAction, display, effAction)
import Control.MultiAlternative (orr)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay, never)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Utils (WidgetOp(..), affWidget, runWidgetAsAff)

widgetSpec :: Spec Unit
widgetSpec =
  describe "Widget" do
    displaySpec
    effSpec
    affSpec
    orrSpec
    orrAndAffSpec

displaySpec :: Spec Unit
displaySpec =
  describe "display" do
    it "should display initial view" do
      ops <- runWidgetAsAff 100 do
        display "foo" $> unit
      ops `shouldEqual`
        [ InitialView (Just "foo") ]

-- TODO: these test chould be wrong.
effSpec :: Spec Unit
effSpec = do
  describe "eff" do
    it "can make a widget that do nothing" do
      ops <- runWidgetAsAff 100 do
        liftEffect $ pure unit
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView Nothing
        , Result unit
        ]

    it "shouldn't mess the initial view and updat views order" do
      ops <- runWidgetAsAff 100 do
        txt <- liftEffect $ pure "foo"
        display txt
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView Nothing
        , UpdateView "foo"
        ]

affSpec :: Spec Unit
affSpec = do
  describe "aff" do
    it "TODO: name this test" do
      ops <- runWidgetAsAff 100 do
        affWidget (Just "foo") $ delay (Milliseconds 10.0) $> 1
      (ops :: Array (WidgetOp String Int)) `shouldEqual`
        [ InitialView (Just "foo")
        , Result 1
        ]

orrSpec :: Spec Unit
orrSpec = do
  describe "orr" do
    it "TODO: name this test" do
      ops <- runWidgetAsAff 100 do
        orr
          [ display "a"
          , display "b"
          , display "c"
          ]
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView (Just "abc") ]

    it "TODO: name this test" do
      ops <- runWidgetAsAff 100 do
        orr
          [ display "a"
          , orr [ display "b", display "c" ]
          ]
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView (Just "abc") ]

    it "TODO: name this test" do
      ops <- runWidgetAsAff 100 do
        orr
          [ display "a"
          , effAction $ pure unit
          ]
        display "foo"
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView Nothing
        , UpdateView "foo"
        ]

orrAndAffSpec :: Spec Unit
orrAndAffSpec = do
  describe "orr and aff" do
    it "TODO: name this test" do
      ops <- runWidgetAsAff 100 do
        orr
          [ display "a"
          , affWidget (Just "b") $ delay (Milliseconds 10.0)
          ]
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView (Just "ab")
        , Result unit
        ]

    it "TODO: name this test" do
      ops <- runWidgetAsAff 100 do
        orr
          [ display "a"
          , do
              affWidget (Just "b") $ delay (Milliseconds 10.0)
              affWidget (Just "c") $ delay (Milliseconds 10.0)
          ]
      (ops :: Array (WidgetOp String Unit)) `shouldEqual`
        [ InitialView (Just "ab")
        , UpdateView "ac"
        , Result unit
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
