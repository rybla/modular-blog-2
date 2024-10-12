module ComponentIndex where

import Prelude

import ComponentPage as ComponentPage
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe')
import Doc (PageId, fromPageIdToJsonUrl, makePageId')
import Effect (Effect)
import Effect.Aff (Aff)
import Fetch (Method(..), fetch)
import Halogen (Component, defaultEval, liftAff, liftEffect, mkComponent, mkEval, modify_)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD
import Page (Page)
import Type.Proxy (Proxy(..))
import Utility (bug)
import Web.DOM.Document as Web.Document
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.URL as URL
import Web.URL.URLSearchParams as URLSearchParams

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

data Query a = Query a

type Input =
  {
  }

type State =
  { status :: Status
  }

data Output = Output

type HTML = HH.ComponentHTML Action Slots Aff

data Action = Initialize

type Slots :: Row Type
type Slots = ()

data Status
  = Blank
  | InvalidPageId String
  | UnknownPageId PageId
  | InvalidPageJson String
  | ValidPage Page

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

component :: Component Query Input Output Aff
component = mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState {} = { status: Blank }

  eval = mkEval defaultEval
    { initialize = Just Initialize
    , handleAction = case _ of
        Initialize -> do
          url_str <- liftEffect do
            window <- Web.HTML.window
            document <- window # Window.document
            Web.Document.url (document # HTMLDocument.toDocument)
          let url = url_str # URL.fromAbsolute # fromMaybe' \_ -> bug "impossible"
          let sp = url # URL.searchParams
          case sp # URLSearchParams.get "pageId" of
            Nothing -> pure unit
            Just pageId_str -> case makePageId' pageId_str of
              Nothing -> pure unit
              Just pageId -> do
                response <- fetch (pageId # fromPageIdToJsonUrl) { method: GET } # liftAff
                if response.ok then do
                  page_str <- response.text # liftAff
                  case page_str # fromJsonString of
                    Left err -> modify_ _ { status = InvalidPageJson (printJsonDecodeError err) }
                    Right page -> modify_ _ { status = ValidPage page }
                else
                  modify_ _ { status = UnknownPageId pageId }
          pure unit
    }

  render { status } =
    HH.div
      []
      ( case status of
          Blank -> [ HH.div [] [ HH.text "<Blank>" ] ]
          InvalidPageId pageId_str -> [ HH.div [] [ HH.text $ "invalid PageId: " <> show pageId_str ] ]
          UnknownPageId pageId -> [ HH.div [] [ HH.text $ "unknown PageId: " <> show pageId ] ]
          InvalidPageJson err -> [ HH.div [] [ HH.text $ "invalid Page JSON: " <> err ] ]
          ValidPage page -> [ HH.slot_ (Proxy :: Proxy "page") unit ComponentPage.component { page } ]
      )

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)
