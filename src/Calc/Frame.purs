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

import Calc.Key as Key
import Calc.Button as Button

data Op = Add
        | Sub
        | Mul
        | Div

type State = { last    :: Maybe String
             , current :: Maybe String
             , op      :: Maybe Op
             }

data Query a = ButtonClicked Key.Key a

type Input = Unit
type Output = Unit

data Slot = ButtonSlot Key.Key
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

makeButtonSlot :: forall m. Key.Key -> H.ParentHTML Query Button.Query Slot m
makeButtonSlot k = HH.slot (ButtonSlot k) Button.button k (\(Button.ButtonClicked k) -> Just $ ButtonClicked k unit)

frame :: forall m. H.Component HH.HTML Query Input Output m
frame = H.parentComponent { initialState: const initialState
                          , render: render
                          , eval: eval
                          , receiver: const Nothing
                          }
        where
          initialState :: State
          initialState = {last: Nothing, current: Nothing, op: Nothing}

          render :: State -> H.ParentHTML Query Button.Query Slot m
          render ({last, current}) = HH.div_
                     [
                       HH.table_
                       [
                         HH.tr [] [HH.text $ showStr last]
                       , HH.tr [] [HH.text $ showStr current]
                       , HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Key.Number 1]
                         , HH.td_ [makeButtonSlot $ Key.Number 2]
                         , HH.td_ [makeButtonSlot $ Key.Number 3]
                         , HH.td_ [makeButtonSlot Key.Add]
                         ]
                       , HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Key.Number 4]
                         , HH.td_ [makeButtonSlot $ Key.Number 5]
                         , HH.td_ [makeButtonSlot $ Key.Number 6]
                         , HH.td_ [makeButtonSlot Key.Sub]
                         ]
                       , HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Key.Number 7]
                         , HH.td_ [makeButtonSlot $ Key.Number 8]
                         , HH.td_ [makeButtonSlot $ Key.Number 9]
                         , HH.td_ [makeButtonSlot Key.Mul]
                         ]
                       , HH.tr_
                         [
                           HH.td_ [makeButtonSlot $ Key.Number 0]
                         , HH.td_ [makeButtonSlot $ Key.Decimal]
                         , HH.td_ [makeButtonSlot $ Key.Equals]
                         , HH.td_ [makeButtonSlot Key.Div]
                         ]
                       ]
                     ]
            where showStr :: Maybe String -> String
                  showStr (Just s) = s
                  showStr Nothing  = "0"

          eval :: Query ~> H.ParentDSL State Query Button.Query Slot Output m
          eval = case _ of
            ButtonClicked k qp -> pure qp
