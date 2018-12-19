module Calc.Frame where

import Data.Maybe
import Prelude

import Data.Int as Int
import Data.Number as Number
import Data.String as String

import Calc.Button as Button
import Calc.Key as Key
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ

data Op = Add
        | Sub
        | Mul
        | Div

type State = { last    :: Maybe String
             , current :: Maybe String
             , op      :: Maybe Op
             }

data Query a = DigitClicked Int a
             | DecimalClicked a
             | OperatorClicked Key.Key a
             | EqualsClicked a

type Input = Unit
type Output = Unit

data Slot = ButtonSlot Key.Key
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

-- ButtonClicked k unit
makeButtonSlot :: forall m. Key.Key -> H.ParentHTML Query Button.Query Slot m
makeButtonSlot k = HH.slot (ButtonSlot k) Button.button k (\(Button.ButtonClicked k) -> Just $ genQueryForKey k)
  where genQueryForKey k =
          case k of
            Key.Number n -> DigitClicked n unit
            Key.Decimal  -> DecimalClicked unit
            Key.Add      -> OperatorClicked k unit
            Key.Sub      -> OperatorClicked k unit
            Key.Mul      -> OperatorClicked k unit
            Key.Div      -> OperatorClicked k unit
            Key.Equals   -> EqualsClicked unit
            
          

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
                         HH.tr_
                         [
                           HH.td [HP.attr (HH.AttrName "colspan") "4"] [HH.text $ showStr last]
                         ]
                       , HH.tr_
                         [
                           HH.td [HP.attr (HH.AttrName "colspan") "4"] [HH.text $ showStr current]
                         ]
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
            DigitClicked n qp -> do
              { last: last, current: maybeCurrent, op: op } <- H.get
              let current = case maybeCurrent of
                              Just c  -> c
                              Nothing -> ""
              H.put { last: last, current: Just $ current <> show n, op: op }
              pure qp
            DecimalClicked qp -> do
              { last: last, current: maybeCurrent, op: op } <- H.get
              let current = case maybeCurrent of
                              Just c  -> c
                              Nothing -> ""
              let new = if String.contains (String.Pattern ".") current then current else current <> "."
              H.put { last: last, current: Just $ new, op: op }
              pure qp
            OperatorClicked k qp -> do
              { last: last, current: current, op: op } <- H.get
              H.put { last: current, current: Nothing, op: keyToMaybeOp k }
              pure qp
            EqualsClicked qp -> do
              { last: last, current: current, op: op } <- H.get
              let l = getNumber last
                  c = getNumber current
                  newLast = case op of
                              Just Add -> l + c
                              Just Sub -> l - c
                              Just Mul -> l * c
                              Just Div -> l / c
                              Nothing  -> c
              H.put { last: Just $ show newLast, current: Nothing, op: Nothing }
              pure qp

                where keyToMaybeOp :: Key.Key -> Maybe Op
                      keyToMaybeOp k = case k of
                                         Key.Add -> Just Add
                                         Key.Sub -> Just Sub
                                         Key.Mul -> Just Mul
                                         Key.Div -> Just Div
                                         _       -> Nothing
                      getNumber :: Maybe String -> Number
                      getNumber maybeS = case maybeS of
                                           Just s  -> case Number.fromString s of
                                                        Just n  -> n
                                                        Nothing -> 0.0
                                           Nothing -> 0.0
