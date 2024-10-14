module ComponentIndex where

import Prelude

import ComponentPage as ComponentPage
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Doc (Page, PageId(..), fromPageIdToJsonUrl, fromPageIdToString, makePageId')
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Fetch (Method(..), fetch)
import Foreign (unsafeToForeign)
import Halogen (Component, defaultEval, liftAff, liftEffect, mkComponent, mkEval, modify_)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Type.Proxy (Proxy(..))
import Utility (bug)
import Web.DOM.Document as Web.Document
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.History as History
import Web.HTML.Location as Location
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

data Action
  = Initialize
  | Update
  -- | GotoPage PageId
  | SetPage PageId
  | GotoArchive
  | SetStatus Status

derive instance Generic Action _

instance Show Action where
  show x = genericShow x

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
  | Loading String

instance Show Status where
  show Blank = "Blank"
  show (Archive _) = "Archive"
  show (InvalidPageId _) = "InvalidPageId"
  show (UnknownPageId _) = "UnknownPageId"
  show (InvalidPageJson _) = "InvalidPageJson"
  show (ValidPage _ _) = "ValidPage"
  show (MiscError _) = "MiscError"
  show (Loading _) = "Loading"

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
    , receive = const (Just Initialize)
    , handleAction = handleAction
    }

  handleAction action@Initialize = do
    Console.log $ "[" <> show action <> "]"
    handleAction Update

  handleAction action@Update = do
    Console.log $ "[" <> show action <> "]"
    window <- Web.HTML.window # liftEffect
    document <- window # Window.document # liftEffect
    url_str <- Web.Document.url (document # HTMLDocument.toDocument) # liftEffect
    let url = url_str # URL.fromAbsolute # fromMaybe' \_ -> bug "impossible"
    let sp = url # URL.searchParams
    case sp # URLSearchParams.get "pageId" of
      Nothing -> handleAction GotoArchive
      Just pageId_str -> case makePageId' pageId_str of
        Nothing -> handleAction $ SetStatus (InvalidPageId pageId_str)
        Just pageId -> handleAction $ SetPage pageId

  handleAction action@(SetPage pageId) = do
    Console.log $ "[" <> show action <> "]"
    handleAction $ SetStatus (Loading $ "loading page " <> show pageId <> " ...")
    response <- fetch (pageId # fromPageIdToJsonUrl) { method: GET } # liftAff
    if not response.ok then handleAction $ SetStatus (UnknownPageId pageId)
    else do
      page_str <- response.text # liftAff
      case page_str # fromJsonString of
        Left err -> handleAction $ SetStatus (InvalidPageJson (printJsonDecodeError err))
        Right page -> handleAction $ SetStatus (ValidPage pageId page)

  -- handleAction action@(GotoPage pageId) = do
  --   Console.log $ "[" <> show action <> "]"
  --   let reload_window = false
  --   if reload_window then do
  --     window <- Web.HTML.window # liftEffect
  --     document <- window # Window.document # liftEffect
  --     url_str <- Web.Document.url (document # HTMLDocument.toDocument) # liftEffect
  --     let url = url_str # URL.fromAbsolute # fromMaybe' \_ -> bug "impossible"
  --     let sp = url # URL.searchParams
  --     let sp' = URLSearchParams.set "pageId" (pageId # fromPageIdToString) sp
  --     let url' = URL.setSearch (sp' # URLSearchParams.toString) url
  --     Console.log (show { url' })
  --     location <- window # Window.location # liftEffect
  --     location # Location.setHref (url' # URL.toString) # liftEffect
  --   else do
  --     handleAction $ SetPage pageId

  handleAction action@GotoArchive = do
    Console.log $ "[" <> show action <> "]"
    handleAction $ SetStatus (Loading $ "loading archive ...")
    response <- fetch "pageIds.json" { method: GET } # liftAff
    if not response.ok then handleAction $ SetStatus (MiscError "can't find pages.json")
    else do
      pageIds_str <- response.text # liftAff
      case pageIds_str # fromJsonString :: _ (Array PageId) of
        Left err -> handleAction $ SetStatus (MiscError $ "error when decoding pageIds.json: " <> show err)
        Right pageIds -> handleAction $ SetStatus (Archive pageIds)

  handleAction action@(SetStatus status) = do
    Console.log $ "[" <> show action <> "]"
    modify_ _ { status = status }

    window <- Web.HTML.window # liftEffect
    document <- window # Window.document # liftEffect
    url_str <- Web.Document.url (document # HTMLDocument.toDocument) # liftEffect
    let url = url_str # URL.fromAbsolute # fromMaybe' \_ -> bug "impossible"
    let sp = url # URL.searchParams
    history <- window # Window.history # liftEffect

    case status of
      Archive _ -> do
        let sp' = sp # URLSearchParams.delete "pageId"
        let url' = url # URL.setSearch (sp' # URLSearchParams.toString)
        History.replaceState ("" # unsafeToForeign) ("TODO: document title" # wrap) (url' # URL.href # wrap) history # liftEffect
      ValidPage pageId _ -> do
        let sp' = sp # URLSearchParams.set "pageId" (pageId # fromPageIdToString)
        let url' = url # URL.setSearch (sp' # URLSearchParams.toString)
        History.replaceState ("" # unsafeToForeign) ("TODO: document title" # wrap) (url' # URL.href # wrap) history # liftEffect
      _ -> pure unit

  render { status } =
    let
      style_index = "width: 100vw; display: flex; flex-direction: column; gap: 1em; align-items: center; margin: 1em 0; "
      style_siteName = "cursor: pointer; text-align: center; font-size: 2em; box-shadow: 0 0 0 1px violet; "
      style_pageTitle = "text-align: center; font-size: 3em; box-shadow: 0 0 0 1px violet; "
      style_content = "display: flex; flex-direction: column; gap: 1.0em; max-width: min(calc(100vw - 1em), 40em); box-shadow: 0 0 0 1px violet; "
    in
      HH.div
        [ HP.classes [ HH.ClassName "index" ]
        , HP.style style_index
        ]
        ( [ [ HH.div
                [ HP.style style_siteName
                , HE.onClick (const GotoArchive)
                ]
                [ HH.text "modular-blog-2" ]
            ]
          , case status of
              Blank ->
                [ HH.div
                    [ HP.style style_pageTitle ]
                    [ HH.text "Blank" ]
                ]
              Archive pageIds ->
                [ HH.div
                    [ HP.style style_pageTitle ]
                    [ HH.text "Archive" ]
                , HH.div
                    [ HP.style style_content ]
                    ( pageIds <#> \pageId@(PageId pageId_str) ->
                        HH.button
                          [ HE.onClick (const (SetPage pageId)) ]
                          [ HH.text pageId_str ]
                    )
                ]
              InvalidPageId pageId_str ->
                [ HH.div
                    [ HP.style style_pageTitle ]
                    [ HH.text "Error" ]
                , HH.div
                    [ HP.style style_content ]
                    [ HH.text $ "invalid PageId: " <> show pageId_str ]
                ]
              UnknownPageId pageId ->
                [ HH.div
                    [ HP.style style_pageTitle ]
                    [ HH.text "Error" ]
                , HH.div
                    [ HP.style style_content ]
                    [ HH.text $ "unknown PageId: " <> show pageId ]
                ]
              InvalidPageJson err ->
                [ HH.div
                    [ HP.style style_content ]
                    [ HH.div
                        [ HP.style style_pageTitle ]
                        [ HH.text "Error" ]
                    , HH.text $ "invalid Page JSON: " <> err
                    ]
                ]
              MiscError err ->
                [ HH.div
                    [ HP.style style_pageTitle ]
                    [ HH.text "Error" ]
                , HH.div
                    [ HP.style style_content ]
                    [ HH.text $ "miscellaneous error: " <> err ]
                ]
              ValidPage pageId page ->
                [ HH.div
                    [ HP.style style_pageTitle ]
                    [ HH.text (fromPageIdToString pageId) ]
                , HH.div
                    [ HP.style style_content ]
                    [ HH.slot_ (Proxy :: Proxy "page") unit ComponentPage.component { page } ]
                ]
              Loading msg ->
                [ HH.div
                    [ HP.style style_pageTitle ]
                    [ HH.text "Loading" ]
                , HH.div
                    [ HP.style style_content ]
                    [ HH.text $ msg ]
                ]
          ] # fold
        )

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)
