module ComponentDoc where

import Doc
import Prelude

import Data.Foldable (fold, intercalate)
import Effect.Aff (Aff)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

data Query a = Query a

type Input =
  { doc :: Doc
  }

type State =
  { doc :: Doc
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
  initialState { doc } = { doc }

  eval = mkEval defaultEval

  render { doc } =
    HH.div
      []
      ( [ renderDoc doc
        ] # fold
      )

--------------------------------------------------------------------------------
-- renderDoc
--------------------------------------------------------------------------------

renderDoc :: Doc -> Array HTML
renderDoc (String str) =
  [ HH.div
      []
      [ HH.text str ]
  ]
renderDoc (Group group) =
  [ HH.div
      [ HP.style
          ( [ case group.style of
                Column -> []
                Row -> []
            ] # fold # intercalate "; "
          )
      ]
      (group.kids # map renderDoc # fold)
  ]
