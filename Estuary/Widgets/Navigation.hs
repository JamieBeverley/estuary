{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Navigation where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Foreign
import Estuary.Tidal.Types
import Estuary.Widgets.Generic
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Control.Monad (liftM)
import Data.Map
import Text.Read
import Text.JSON
import Data.Time.Clock

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Hint
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Terminal

import Estuary.Widgets.View


data Navigation =
  Splash |
  TutorialTypesList |
  TutorialListPage String | -- String - type of tutorial; ie. Sturcture editor, vs text, vs cquenze etc...
  Tutorial String String | -- name, type -- maybe this should include its type too so we don't have things conflicting btwn tutorials - like if we had an "intro" for Cquenze and for tidal text
  Solo |
  Lobby |
  CreateEnsemblePage |
  CreateTutorialPage String | -- String is for tutorial type
  Collaborate String


navigation :: MonadWidget t m => UTCTime -> Event t Command -> Event t [ServerResponse] ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint)
navigation now commands wsDown = mdo
  let initialPage = page commands wsDown now Splash
  let rebuild = fmap (page commands wsDown now) navEvents
  w <- widgetHold initialPage rebuild
  values <- liftM joinDyn $ mapDyn (\(x,_,_,_)->x) w
  wsUp <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_,_)->x) w
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x,_)->x) w
  navEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,_,_,x)->x) w
  return (values,wsUp,hints)

page :: MonadWidget t m => Event t Command -> Event t [ServerResponse] -> UTCTime -> Navigation ->
  m (Dynamic t [TransformedPattern],Event t ServerRequest,Event t Hint,Event t Navigation)

page _ wsDown _ Splash = do
  x <- liftM (TutorialTypesList <$) $ el "div" $ button "Tutorials"
  y <- liftM (Solo <$)  $ el "div" $ button "Solo"
  z <- liftM (Lobby <$)  $ el "div" $ button "Collaborate"
  let navEvents = leftmost [x,y,z]
  return (constDyn [],never,never,navEvents)

page _ wsDown _ TutorialTypesList = do
  el "div" $ text "Click on a button to select a tutorial interface:"
  t1 <- liftM (TutorialListPage "Structure editing" <$) $ el "div" $ button "Structure editing"
  t2 <- liftM (TutorialListPage "TidalCycles text editing" <$) $ el "div" $ button "TidalCycles text editing"
  back <- liftM (Splash <$) $ button "Return to splashscreen"
  let navEvents = leftmost [t1,t2,back]
  return (constDyn [],never,never,navEvents)

page _ wsDown _ (TutorialListPage t) = do   -- where 't' refers to type of tutorial; lke "structure editing" vs. "text editing"
  requestTutorialList <- liftM (GetTutorialList t <$) getPostBuild   -- Added  'GetTutorialList  String' to  data Request (Types/Request.hs)
  spaceList <- holdDyn [ ] $ fmapMaybe justTutorialList wsDown -- Added justTutorialList to Response.hs
  text (t++" tutorials:")
  join  <- simpleList spaceList (navButton Tutorial) -- added Tutorial to types in this file...
  join' <- mapDyn leftmost join
  let join'' = switchPromptlyDyn join'
  create <- liftM (CreateTutorialPage t <$ ) $ el "div" $ button "Create new tutorial" -- added "CreateTutorialPage "string"" to navigation type and page widget
  back <- liftM (TutorialTypesList  <$) $ el "div" $ button "back to all tutorials"
  return (constDyn [],requestTutorialList,never,leftmost [back,join'',create])


page commands wsDown now (Tutorial s t) = do
  (defMap,wsUp,hints) <- viewInTutorialWidget s t now commands wsDown -- TODO make viewInTutorialWidget
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (TutorialListPage t <$) $ button "back to tutorials"
  return (patterns,wsUp,hints,x)

page _ wsDown _ (Tutorial _ _) = do
  text "Oops... a software error has occurred and we can't bring you to the tutorial you wanted! If you have a chance, please report this as a bug on Estuary's github site"
  x <- liftM (Splash <$) $ button "back to splash"
  return (constDyn [],never,never,x)

page _ _ _ (CreateTutorialPage s) = do
  el "div" $ text ("Create a new "++s++" tutorial")
  el "div" $ text "Note: To successfully create a tutorial  you need to know and enter the correct admin password."
  adminPwd <- el "div" $ do
    text "Admin Password: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  name <- el "div" $ do
    text "Tutorial Name: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  password <- el "div" $ do
    text "Tutorial Password: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  nameAndPassword <- combineDyn (,) name password
  confirm <- el "div" $ button "Confirm"
  let createTutorial = fmap (\(a,b) -> CreateTutorial a b s) $ tagDyn nameAndPassword confirm
  let authenticateAdmin = fmap Authenticate $ updated adminPwd
  cancel <- el "div" $ button "Cancel"
  let serverRequests = leftmost [createTutorial,authenticateAdmin]
  let navEvents = fmap (const $ TutorialListPage s) $ leftmost [cancel,() <$ createTutorial]
  return (constDyn [], serverRequests, never, navEvents)

page _ wsDown _ Solo = do
  (defMap,hints) <- viewInSoloWidget standardView
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Splash <$) $ button "Return to splashscreen"
  return (patterns,never,hints,x)


page _ wsDown _ Lobby = do
  requestEnsembleList <- liftM (GetEnsembleList <$) getPostBuild
  spaceList <- holdDyn [] $ fmapMaybe justEnsembleList wsDown
  join <- simpleList spaceList joinButton -- m (Dynamic t [Event t Navigation])
  join' <- mapDyn leftmost join -- m (Dynamic t (Event t Navigation))
  let join'' = switchPromptlyDyn join' -- Event t Navigation
  create <- liftM (CreateEnsemblePage <$) $ el "div" $ button "Create New Ensemble"
  back <- liftM (Splash <$) $ el "div" $ button "back to splash"
  return (constDyn [],requestEnsembleList,never,leftmost [back,join'',create])

page _ _ _ CreateEnsemblePage = do
  el "div" $ text "Create A New Ensemble"
  el "div" $ text "Note: To successfully create an ensemble you need to know and enter the correct admin password."
  adminPwd <- el "div" $ do
    text "Admin Password: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs & textInputConfig_inputType .~ "password"
  name <- el "div" $ do
    text "Ensemble Name: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  password <- el "div" $ do
    text "Ensemble Password: "
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    liftM _textInput_value $ textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
  nameAndPassword <- combineDyn (,) name password
  confirm <- el "div" $ button "Confirm"
  let createEnsemble = fmap (\(a,b) -> CreateEnsemble a b) $ tagDyn nameAndPassword confirm
  let authenticateAdmin = fmap Authenticate $ updated adminPwd
  cancel <- el "div" $ button "Cancel"
  let serverRequests = leftmost [createEnsemble,authenticateAdmin]
  let navEvents = fmap (const Lobby) $ leftmost [cancel,() <$ createEnsemble]
  return (constDyn [], serverRequests, never, navEvents)

page commands wsDown now (Collaborate w) = do
  (defMap,wsUp,hints) <- viewInEnsembleWidget w now commands wsDown
  patterns <- mapDyn (justStructures . elems) defMap
  x <- liftM (Lobby <$) $ button "back to lobby"
  return (patterns,wsUp,hints,x)


joinButton :: MonadWidget t m => Dynamic t String -> m (Event t Navigation)
joinButton x = do
  b <- clickableDivClass'' x "placeholderClass" ()
  return $ Collaborate <$> tagDyn x b

navButton:: MonadWidget t m => (String->Navigation) ->  Dynamic t String -> m (Event t Navigation)
navButton constructor label = do
  b <- clickableDivClass'' label "placeholderClass" ()
  return $ constructor <$> tagDyn label b

{-
tempoWidget :: MonadWidget t m => Event t [ServerResponse] -> m (Event t ServerRequest)
tempoWidget deltas = do
  text "CPS:"
  let delta' = fmap (Prelude.filter isCps) deltas
  let delta'' = fmapMaybe lastOrNothing delta'
  let delta''' = fmapMaybe getCps delta''
  let delta'''' = fmap show delta'''
  t <- textInput $ def & textInputConfig_setValue .~ delta''''
  let t' = fmapMaybe (readMaybe) $ _textInput_input t
  let edits = fmap (TempoChange "") t'
  return edits
-}

diagnostics :: MonadWidget t m =>
  Dynamic t (Map Int TransformedPattern) ->
  Event t ServerRequest ->
  Event t [ServerResponse] ->
  Event t Hint ->
  m ()
diagnostics values deltasUp deltasDown hints = do
  el "div" $ do
    text "Values:"
    mapDyn encode values >>= display
  el "div" $ do
    text "DeltasUp:"
    (holdDyn "" $ fmap encode deltasUp) >>= display
  el "div" $ do
    text "DeltasDown:"
    (holdDyn "" $ fmap encode deltasDown) >>= display
  el "div" $ do
    text "Hints:"
    (holdDyn "" $ fmap show hints) >>= display
