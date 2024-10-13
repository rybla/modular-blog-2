module ComponentIndex where

import Prelude

import ComponentPage as ComponentPage
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe')
import Doc (Page, PageId(..), fromPageIdToJsonUrl, fromPageIdToString, makePageId')
import Effect (Effect)
import Effect.Aff (Aff)
import Fetch (Method(..), fetch)
import Halogen (Component, defaultEval, liftAff, liftEffect, mkComponent, mkEval, modify_)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
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
  | Archive (Array PageId)
  | InvalidPageId String
  | UnknownPageId PageId
  | InvalidPageJson String
  | ValidPage PageId Page
  | MiscError String

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
            Nothing -> do
              response <- fetch "pageIds.json" { method: GET } # liftAff
              if not response.ok then modify_ _ { status = MiscError "can't find pages.json" }
              else do
                pageIds_str <- response.text # liftAff
                case pageIds_str # fromJsonString :: _ (Array PageId) of
                  Left err -> modify_ _ { status = MiscError $ "error when decoding pageIds.json: " <> show err }
                  Right pageIds -> modify_ _ { status = Archive pageIds }
            Just pageId_str -> case makePageId' pageId_str of
              Nothing -> modify_ _ { status = InvalidPageId pageId_str }
              Just pageId -> do
                response <- fetch (pageId # fromPageIdToJsonUrl) { method: GET } # liftAff
                if not response.ok then modify_ _ { status = UnknownPageId pageId }
                else do
                  page_str <- response.text # liftAff
                  case page_str # fromJsonString of
                    Left err -> modify_ _ { status = InvalidPageJson (printJsonDecodeError err) }
                    Right page -> modify_ _ { status = ValidPage pageId page }
          pure unit
    }

  render { status } =
    HH.div
      []
      ( case status of
          Blank ->
            [ HH.h1_ [ HH.text "Blank" ]
            ]
          Archive pageIds ->
            [ HH.h1_ [ HH.text "Archive" ]
            , HH.div
                []
                (pageIds <#> \pageId@(PageId pageId_str) -> HH.a [ HP.href $ "?pageId=" <> pageId_str ] [ HH.text $ show pageId ])
            ]
          InvalidPageId pageId_str ->
            [ HH.h1_ [ HH.text "Error" ]
            , HH.div [] [ HH.text $ "invalid PageId: " <> show pageId_str ]
            ]
          UnknownPageId pageId ->
            [ HH.h1_ [ HH.text "Error" ]
            , HH.div [] [ HH.text $ "unknown PageId: " <> show pageId ]
            ]
          InvalidPageJson err ->
            [ HH.div []
                [ HH.h1_ [ HH.text "Error" ]
                , HH.text $ "invalid Page JSON: " <> err
                ]
            ]
          MiscError err ->
            [ HH.h1_ [ HH.text "Error" ]
            , HH.div [] [ HH.text $ "miscellaneous error: " <> err ]
            ]
          ValidPage pageId page ->
            [ HH.h1_ [ HH.text (fromPageIdToString pageId) ]
            , HH.slot_ (Proxy :: Proxy "page") unit ComponentPage.component { page }
            ]
      )

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)
