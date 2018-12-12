module Calc.Frame where

import Prelude

import Data.Maybe

--import CSS as CSS
--import CSS.TextAlign as CSSTA
--import CSS.VerticalAlign as CSSVA

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.CSS as HCSS
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ

import Calc.Key (Key(..))
import Calc.Button as Button

type State = Unit

data Query2 a = ButtonClicked2 a

type Input = Unit
type Output = Unit

data Slot = ButtonSlot Key
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

makeButtonSlot :: forall m. Key -> H.ParentHTML Query2 Button.Query Slot m
makeButtonSlot k = HH.slot (ButtonSlot k) Button.button k (\_ -> Just $ ButtonClicked2 unit)

frame :: forall m. H.Component HH.HTML Query2 Input Output m
frame = H.parentComponent { initialState: const initialState
                          , render: render
                          , eval: eval
                          , receiver: const Nothing
                          }
        where
          initialState :: State
          initialState = unit

          render :: State -> H.ParentHTML Query2 Button.Query Slot m
          render _ = HH.div_
                     [
                       HH.table_
                       [
                         HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Number 1]
                         , HH.td_ [makeButtonSlot $ Number 2]
                         , HH.td_ [makeButtonSlot $ Number 3]
                         , HH.td_ [makeButtonSlot Add]
                         ]
                       , HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Number 4]
                         , HH.td_ [makeButtonSlot $ Number 5]
                         , HH.td_ [makeButtonSlot $ Number 6]
                         , HH.td_ [makeButtonSlot Sub]
                         ]
                       , HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Number 7]
                         , HH.td_ [makeButtonSlot $ Number 8]
                         , HH.td_ [makeButtonSlot $ Number 9]
                         , HH.td_ [makeButtonSlot Mul]
                         ]
                       , HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Number 0]
                         , HH.td_ [makeButtonSlot $ Decimal]
                         , HH.td_ [makeButtonSlot $ Equals]
                         , HH.td_ [makeButtonSlot Div]
                         ]
                       ]
                     ]

          eval :: Query2 ~> H.ParentDSL State Query2 Button.Query Slot Output m
          eval = case _ of
            ButtonClicked2 qp -> pure qp
