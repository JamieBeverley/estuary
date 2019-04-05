{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Sequencer where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Control.Monad
import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Safe.Foldable (maximumMay)
import Text.Read (readMaybe)
import Estuary.Types.Hint
import Estuary.Widgets.Generic
import Estuary.Types.Context
import Estuary.Types.Tempo

import Sound.MusicW.AudioContext


import Data.Time
import Control.Monad.IO.Class (liftIO)

type Sequence a = Map Int (Maybe a,[Bool])

attachIndex :: [a] -> [(Int,a)]
attachIndex = zip [0..]

-- Inputs:
--    Dynamic t Context                  - Dynamic record-syntax object containing context information such as language (common accross estuary)
--    Map Int (String,[Bool])            - initial map to populate the sequencer - each map entry is a row in the sequencer.
--    Event t (Map Int (String,[Bool]))  - receives events from server to update sequencer rows
-- Output (pretty standard accross Estuary):
--    A MonadWidget (ie. has some browser widget-creating action) 3-tuple containing:
--      - Dynamic t (Map Int (String,[Bool]))   - The value used for immediate local rendering (ie. this what we hear)
--      - Event t (Map Int (String,[Bool]))     - An Event containing the state of the sequencer whenever a LOCAL change is made to the sequencer as a result of
--                                                a direct user interaction (eg. clicking a button). This event gets sent to the server to send it to other connected
--                                                clients. It's important to differentiate this from a change to the local state of the sequencer that resulted
--                                                from a server update. If this event fired when an update was received from the server, then all other connected
--                                                clients would receive another sequencer update synonymous to the one that just fired (and then we get cyclical updates)
--      - Event t Hint                          - An event containing hints about things that could be pre-loaded before they need to be played. Eg. a user types a sample
--                                                name but hasn't evaluated yet - we could load that sample before it is needed (not currently used in this widget).
sequencer ::MonadWidget t m => Dynamic t Context -> Map Int (String, [Bool]) -> Event t (Map Int (String,[Bool])) -> m (Dynamic t (Map Int (String,[Bool]), Event t (Map Int (String,[Bool])), Event t Hint))
sequencer ctx iMap update = elClass "table" "sequencer" $ mdo
-- ^'mdo' eg. RecursiveDo is necessary here. For example, 'deleteEvents' below is generated from a widget that it also feeds into (via 'downstreamEvs')

  let seqLen = maybe 8 id $ maximumMay $ fmap (length . snd) $ elems iMap  -- static count of the length of sequencer (eg. number of buttons in a row)

  -- Row containing sequencer indicator light - needs to take the context for the tempo and seqLen for the length of the indicator row
  sequencerIndicatorRow ctx seqLen

  -- This line is a bit complicated - serverDeletes::Event t (Map Int (String,[Bool]))
  -- 'update' includes edits from the server (ie. someone else editing the sequencer).
  -- the update event (which contains a state of the sequncer) is compared with the current state of the sequencer represented in the
  -- local client's browser (the `current values` expression below).  For elements that appear in the local state but are not present
  -- in 'update', we can infer that someone else deleted a row of the sequencer. this is used below in 'updateEvs'...
  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows

  -- updateEvs:: Event t (Map Int (Maybe (String,[Bool]) ))  - represents server updates in a form that we can use with 'listWithKeyShallowDiff' below
  -- Merges deletions and additions/updates from the server to the state of the sequencer.
  -- mergeWith is a handy function for combining events that happen at the same time - similar to leftmost except you provide a function that is
  -- used to handle the case when two events occur simultaneously. Since 'serverDeletes' is just a modification of 'update' we expect that these
  -- events will indeed occur on the same frame.
  -- They're mapped to Maybe values for reasons that will become more clear when you get to 'listWithKeyShallowDiff'
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]

  -- downstreamEvs::Event t (Map Int (Maybe (String,[Bool])))
  -- merges 3 different events that will affect the state of the sequencer:
  --     updateEvs     - updates from the server
  --     newRow        - when you click the '+' button to create a new sequencer row
  --     deleteEvents  - when you click the '-' button in a sequencer row to delete the row, that row issues an event which is caught
  --                     at this level of the structure, and fed into 'listWithKeyShallowDiff' (see below)
  let downstreamEvs = leftmost [updateEvs, fmap (Just <$>) newRow, fmap (Nothing <$) deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))


  -- widgets:: m (Dynamic t  (Map Int ( (String,[Bool]),  Event () ) ) )
  -- This line builds a list of widgets, each of which is a row of the sequencer
  -- We use listWithKeyShallowDiff frequently when we want a container for some number of 'child' widgets that can grow or shrink - in
  -- this case we have some number of child sequencer rows that can be added to, deleted, or modified.
  -- listWithKeyShallowDiff::
  --                   Map k v ->                         - Initial map of values passed to widget-building function to populate list of widgets. Note both key and value of each
  --                                                        map entry are inputs to the builder function (the 3rd input to listWithKeyShallowDiff)
  --                   Event t (Map k (Maybe v)) ->       - Event containing a map with updates to 'kth' entry in the widget map. The Maybe value is used to
  --                                                        differentiate update and child creation events (Just values) from deletions (Nothing values). Eg. if we wanted
  --                                                        to delete a row of the sequencer we would send an event containing a 'k' mapped to Nothing. If we wanted to
  --                                                        update widget 'k' or insert an new entry 'k' into the map we would map 'k' to a Just value.
  --                   (k -> v -> Event t v -> m a) ->    - a function that takes a map key, value, and update event and produces a widget. In our case each chlid is a sequencer
  --                                                        row which doesn't need information about it's 'key' so this is '(const sequencerRow)'
  --                   m (Dynamic t (Map k a))            - a widget containing a dynamic map of values - each map entry corresponding to one of the child widgets
  --
  -- Frequently our child widgets used in listWithKeyShallowDiff return a dynamic value (ie. the 'a' in the above type is 'Dynamic t b'). This makes the output of
  -- listWithKeyShallowDiff difficult to use: m (Dynamic t (Map k (Dynamic t b)))
  -- Fortunately reflex provids `joinDynThroughMap` to combine the outer dynamic with the one inside the map to produce: m (Dynamic t (Map k b))
  -- joinDynThroughMap is not a monadic function (it has no 'widget building' effect) so we must lift it into the monad with 'liftM'
  widgets <- liftM joinDynThroughMap $ listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow) -- Dyn t (Map Int ((String,[Bool]), Event t ()))

  -- sequencerRow returns a dynamic 3-tuple containing:
  --        (string,[Bool])              - the immediate value of that row used for rendering only
  --        Event t (String,[Bool])      - an event that fires when the user has edited the row locally
  --        Event t ()                   - an event containing () to signal that this row should be deleted
  -- The following three lines extract those three values individually. Note that they contain maps referring to each
  -- individual child (row).
  let values = fmap (fmap (\(x,_,_)-> x)) widgets -- Dyn t (Map Int (String,[Bool]))
  let updateChildren = switch $ current $  fmap (mergeMap . fmap (\(_,x,_)->x)) widgets -- Event t (Map Int (String,[Bool]))
  let deleteEvents = switch $ current $ fmap (mergeMap . fmap (\(_,_,d)->d)) widgets -- Event t (Map Int ())


  -- updateVal:: Event t (Map Int (String,[Bool]))
  -- Event containing the state of the sequencer that fires only when the user makes local changes to the state of the sequencer.
  -- Again, some caution has to be taken here to ensure that this event doesn't trigger when the local sequcner state changes as a result of a
  -- server update - otherwise that server update would be re-propogated back over the server to other clients, which would then again propogate the
  -- event back, etc....
  -- This event combines 3 different local update events:
  --          updateChildren -  events that fire from the sequencer rows (eg. when a button in that row is pressed)
  --          deleteEvents   -  events that fire fro the sequencer rows when one is deleted
  --          newRow         -  when the user clicks '+' to create a new row
  let updateVal = leftmost [attachDynWith (flip M.union) values updateChildren, attachDynWith (M.difference) values deleteEvents, attachDynWith (flip M.union) values newRow]

  -- just encodes the maximum key (an Int) in the Map of sequencer rows. This gets used in 'newRow'; the 'key' of the new row (which is an Int) juse becomes the maxKey
  -- plus 1. This way we can be sure we won't overwrite an existing row in the sequencer when we create a new row
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) $ nubDyn values

  -- Just a button generating an event to create a new row.
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()

  -- uses the current maxkey and the plusButton event to signal the creation of a newRow. newRow feeds into 'downstreamEvs' which feeds into
  -- listWithKeyShallowDiff to make a new child widget (a new row)
  let newRow = attachWith (\k _-> singleton (k+1) (("",take seqLen $ repeat False))) (current maxKey) plusButton

  -- package and wrap values in monad. note the 'Hint' value here is just an event that never fires ('never')
  return $ fmap (\v-> (v,updateVal,never)) values






-- Inputs:
--        (String,[Bool])             - initial value of the row
--        Event t (String,[Bool])     - receive events from parent widget ('sequencer') that are updates to this row from the server
-- Outputs:
--        m (Dynamic t ((String,[Bool]), Event t (String,[Bool]), Event t ()))
--                                    - 3-tuple containing:
--                                                   - a dynamic row value used immediately for rendering only
--                                                   - an Event containing the sequencer row's value that signals a local change to the sequencer row
--                                                     (these propogate to the server to update other clients' sequencers)
--                                                   - an empty event to signal to the parent widget that this row should be deleted.
sequencerRow ::(MonadWidget t m) => (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((String,[Bool]), Event t (String,[Bool]), Event t ()))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  -- Map Int Bool   -> just turning [Bool] into a Map because Maps are easier to work with
  let buttonIVals = M.fromList $ attachIndex vals

  -- Event t Text   - updates from server made to the string of this row (eg. the sample name)
  let strUpdate = fmap (T.pack . fst) edits

  -- Event t (Map Int Bool)  - updates from the server made to the sequencer buttons
  let buttonUpdates = fmap (M.fromList . attachIndex . fmap Just . snd) edits

  -- Map Text Text - used to set the class of the text input where you enter sample names
  let textInputAttrs = singleton "class" "sequencer-textarea code-text"

  -- A simple button to signal the deletion of a row. This event is caught by the sequencerRow's parent widget ('sequencer') to delete it from its structure
  deleteMe <- elClass "td" "delete" $ button "-" -- clickableTdClass (constDyn " - ") (constDyn "delete") ()

  -- a textInput wrapped in an html <td> element (for styling purposes). The text input has the initial value of iVal (T.pack turns a 'String' into 'Text').
  -- textInput also takes a 'setValue' parameter of type 'Event t Text' to update the value of it's text - we set that to changes from the server.
  rowInput <- elClass "td" "sequencerTextInputTd" $ textInput $ def & textInputConfig_initialValue .~ (T.pack iVal) & textInputConfig_setValue .~ strUpdate



  -- widgets:: m (Dynamic t (Map Int (Bool,Event Bool)))
  -- This line builds a list of widgets, each of which is a button of the sequencerRow
  -- We use listWithKeyShallowDiff frequently when we want a container for some number of 'child' widgets that can grow or shrink - in
  -- this case we have some number of 'child' sequencer buttons that belong to a 'parent' sequencerRow. Currently the length of the sequencer doesn't change
  -- (ie. there are 8 sequencer buttons in a row and that doesn't chage), but listWithKeyShallowDiff offers a way to add/remove child elements. See 'sequencer'
  -- for demonstration of that (where sequencerRows maybe added and deleted)
  -- listWithKeyShallowDiff::
  --                   Map k v ->                         - Initial map of values passed to widget-building function to populate list of widgets. Note both key and value of each
  --                                                        map entry are inputs to the builder function (the 3rd input to listWithKeyShallowDiff)
  --                   Event t (Map k (Maybe v)) ->       - Event containing a map with updates to 'kth' entry in the widget map. The Maybe value is used to
  --                                                        differentiate update and child creation events (Just values) from deletions (Nothing values). Eg. if we wanted
  --                                                        to delete a button of the sequencerRow we would send an event containing a 'k' mapped to Nothing. If we wanted to
  --                                                        update widget 'k' or insert an new entry 'k' into the map we would map 'k' to a Just value.
  --                   (k -> v -> Event t v -> m a) ->    - a function that takes a map key, value, and update event and produces a widget. In our case each chlid is a sequencer
  --                                                        button so we use the builder sequencerButton (see below)
  --                   m (Dynamic t (Map k a))            - a widget containing a dynamic map of values - each map entry corresponding to the value of one sequencer button
  --
  -- Frequently our child widgets used in listWithKeyShallowDiff return a dynamic value (ie. the 'a' in the above type is 'Dynamic t b'). This makes the output of
  -- listWithKeyShallowDiff difficult to use: m (Dynamic t (Map k (Dynamic t b)))
  -- Fortunately reflex provids `joinDynThroughMap` to combine the outer dynamic with the one inside the map to produce: m (Dynamic t (Map k b))
  -- joinDynThroughMap is not a monadic function (it has no 'widget building' effect) so we must lift it into the monad with 'liftM' (since listWithKeyShallowDiff is monadic)
  --
  widgets <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals buttonUpdates sequencerButton  -- Dyn (Map Int (Bool, Event Bool))

  -- sequencerButton (the builder function passed to listWithKeyShallowDiff) returns 2 values: a Bool to be used for immediate rendering and an update Event signal
  -- to the server that an edit has occurred.
  -- The following 3 lines just separate 'widgtes' from: Dyn (Map Int (Bool, Ev Bool))) into:
  --                               buttons:         Dyn [Bool] and
  --                               buttonEdits:     Ev (Map Int Bool))
  -- (along with some type 'massaging')
  let buttons = fmap (elems . fmap fst) widgets -- Dyn (Map Int Bool)
  let buttonEdits = switchPromptlyDyn $ fmap (mergeMap . fmap snd) widgets -- Event (Map Int Bool)
  let buttonEdits' = tagDyn buttons buttonEdits -- not sure if this is really necessary... probably shouldn't be.

  -- strEdits: Event t String   - just the dynamic value of the text input
  let strEdits = fmap T.unpack $ _textInput_input rowInput

  -- updates:: Event t (String,[Bool])
  -- updates sent to parent widget to propogate to server (ie. local edits made to either the text or buttons from this row)
  -- This line essentially combines two events:
  --          buttonEdits' - events that trigger when a sequencer button is toggled on/off
  --          strEdits     - events that trigger when the sample name is changed
  -- When buttonEdits' fires we tag that event with the current state of the sample text (so we get 'Event (String,[Bool])'), and when strEdits fires we tag the event with
  -- the current state of the buttons (again to get 'Event (string,[Bool])')
  let updates = leftmost [attachPromptlyDyn (fmap (T.unpack) $ _textInput_value rowInput) buttonEdits', attachPromptlyDynWith (flip (,)) (buttons) strEdits]

  -- 'val' just combines the dynamic text value of the textinput with the dynamic state of the sequencerRow's buttons
  val <- combineDyn (\s b -> (T.unpack s, b)) (_textInput_value rowInput) buttons

  -- pack the values in a 3-tuple and return them
  mapDyn (\x->(x, updates,deleteMe)) val





-- Simple sequencer button widget.
-- Input:
--        Int ->         - an unused Int parameter (which may be more clear when you look at the listWithKeyShallowDiff in sequencerRow - don't worry about it for now)
--       Bool ->         - initial state of the button (toggled on or off)
--      Event t Bool ->  -  receives an Event to change the state of the button (ie. a server update - someone else making a change to the sequencer)
-- Output:
--       m(Dynamic t (Bool, Event t Bool))
--                       - First 'Bool' represents the current state of the button used immediately for rendering
--                       - Event t Bool - an event the fires only when a local change to the button has occured (ie. when a user clicks it locally - not a change to the
--                         button that resulted from a server update)
sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t (Bool, Event t Bool))
sequencerButton _ val edits = mdo
  -- create a <td> element and retain the 'el' reference in element. 'attrs' is a dynamic map that sets the style of the button according to its state (see below)
  -- since we derrive 'attrs' further down in this do block we need to use a 'RecursiveDo' - that's what the 'mdo' is on the line above
  (element,_) <- elDynAttr' "td" attrs $ return ()
  -- using 'element' from our <td> we can get click events on that td using 'wrapDomEvent' and the event tag 'Mousedown' - we can also get all other types of events (mouse events, key events, etc...) with a similiar syntax
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Mousedown) (return ())

  -- each time the button is clicked we check the current state of the button ('current isActive') and create a new event that is the opposite value of the current
  -- active value (so each button press toggles the button to a different value from what it is currently)
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]

  -- isActive contains the immediate dynamic 'bool' state of the button. it is initially set to 'val' and then gets updated by server events and user clicks
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]

  -- the styling of the button changes in response to it's state. attrs:: m (Dyn (Map Text Text)) encodes this and is fed into elDynAttr' above
  attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButton sequencer-pads-activated" else "sequencerButton sequencer-pads") isActive

  -- return the button state and the update event
  return $ fmap (\x->(x,clickUpdates)) isActive






-- Indicator row widgets
sequencerIndicatorRow:: MonadWidget t m => Dynamic t Context -> Int -> m ()
sequencerIndicatorRow ctx seqLen = do
  beatEv <- beatCounter (fmap tempo ctx) seqLen
  elClass "tr" "sequencerRow" $ do
    elClass "td" "sequencerIndicatorSpacer" $ return ()
    classes <- holdDyn (take seqLen $ repeat "sequencerIndicator") $ fmap (\a -> (take a $ repeat "sequencerIndicator")++"sequencerIndicatorActivated":(take (seqLen-1-a) $ repeat "sequencerIndicator")) beatEv -- Dyn t [String]
    classMap <- mapDyn (fromList . zip [0::Int, 1..]) classes -- Dyn map Int STring
    listWithKey classMap sequenceIndicator
    return ()

beatCounter :: MonadWidget t m => Dynamic t Tempo -> Int -> m (Event t Int)
beatCounter t seqLen = do
  startT <- liftIO getCurrentTime
  pb <- getPostBuild
  let tickBuilder = fmap (\x -> tickLossy (realToFrac (1.0/(fromIntegral seqLen)/2/(cps x))) startT) $ leftmost [updated t, tagPromptlyDyn t pb]
  tick <- liftM switchDyn $ widgetHold (return never) tickBuilder
  nowT <- performEvent $ fmap (const $ liftAudioIO audioUTCTime) tick
  let ellapsedCyc = attachDynWith elapsedCycles t nowT -- Event t Double
  let beatEv = fmap (\x-> mod (round $ (x - (fromIntegral $ floor x)) * (fromIntegral seqLen)) seqLen) ellapsedCyc
  liftM (updated . nubDyn) $ holdDyn 0 beatEv

sequenceIndicator:: MonadWidget t m => Int -> Dynamic t Text -> m ()
sequenceIndicator _ c = do
  attrs <- mapDyn (singleton "class") c
  elDynAttr "td" attrs $ return ()
-- >>>>>>> 2c3238f7106baff292e5c4506997fee3a1b02628
