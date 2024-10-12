module ComponentPage where

import Doc
import Page
import Prelude

import ComponentDoc as ComponentDoc
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Effect.Aff (Aff)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

data Query a = Query a

type Input =
  { page :: Page
  }

type State =
  { page :: Page
  }

data Output = Output

type HTML = HH.ComponentHTML Action Slots Aff

data Action

type Slots :: Row Type
type Slots = ()

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

component :: Component Query Input Output Aff
component = mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState { page } = { page }

  eval = mkEval defaultEval

  render { page: Page { abstract, kids } } =
    HH.div
      []
      ( [ [ HH.div
              []
              [ HH.div
                  []
                  [ HH.text "abstract" ]
              , HH.slot_ (Proxy :: Proxy "abstract") unit ComponentDoc.component { doc: abstract }
              ]
          ]
        , kids # mapWithIndex \i doc -> HH.slot_ (Proxy :: Proxy "doc") i ComponentDoc.component { doc }
        ] # fold
      )

