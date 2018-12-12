module Calc.Button
       ( button
       , Query(..)
       , Output(..)
       ) where

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
import Halogen.VDom.Driver (runUI)

import Calc.Key (Key)

type State = Key

data Query a = ClickButton Key a

type Input = Key
data Output = ButtonClicked Key

button :: forall m. H.Component HH.HTML Query Input Output m
button = H.component { initialState: initialState 
                     , render: render
                     , eval: eval
                     , receiver: const Nothing
                     }
        where
          initialState :: Input -> State
          initialState k = k

          render :: State -> H.ComponentHTML Query
          render k = HH.button [HE.onClick (HE.input_ $ ClickButton k)] [HH.text $ show k]

          eval :: Query ~> H.ComponentDSL State Query Output m
          eval = case _ of
            ClickButton k qp -> do
              H.raise $ ButtonClicked k
              pure qp
