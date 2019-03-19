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

attachIndex:: [a] -> [(Int,a)]
attachIndex l = zip (take (length l) [0..]) l



sequencer' ::MonadWidget t m => Dynamic t Context -> Map Int (String, [Bool]) -> Event t (Map Int (String,[Bool])) -> m (Dynamic t (Map Int (String,[Bool])), Event t Hint)
sequencer' ctx iMap update = elClass "table" "sequencer" $ mdo
  let seqLen = maybe 8 id $ maximumMay $ fmap (length . snd) $ elems iMap

  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]
  let downstreamEvs = leftmost [updateEvs, newRow, deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))
  sequencerIndicatorRow ctx seqLen

-- listWithKeyShallowDiff :: (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))

  widgets <- liftM joinDynThroughMap $ listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow) -- Dyn t (Map Int ((String,[Bool]), Event t ()))
  values <- mapDyn (fmap fst) widgets -- Dyn t (Map Int (String,[Bool]))
  dynEvs <- mapDyn (mergeMap . fmap (snd)) widgets -- Event t (Map Int ())
  let deleteEvents = fmap (Nothing <$) $ switch $ current $ dynEvs
  let updateVal = fmapMaybe id $ mergeWith (\a b->Nothing) [Nothing <$ updateEvs, Just <$> updated (nubDyn values)]
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) $ nubDyn values
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (Just ("",take seqLen $ repeat False))) (current maxKey) plusButton
  values' <- holdDyn iMap updateVal
  debug $ fmap (("values update: " ++) . show)$ updated values
  debug $ fmap (("updateVals" ++) . show) updateVal
  return (values', never)


blockServerUpdates ::(Eq a, MonadWidget t m)=> Event t a -> Dynamic t a -> m (Event t a)
blockServerUpdates update localState = do
  let e = mergeWith (\_ _ -> Nothing) [fmap (const Nothing) update, fmap Just $ updated $ nubDyn localState]
  return $ fmapMaybe id e


  --
  -- updateDyn <- holdDyn Nothing $ fmap Just update -- Dyn (Maybe a)
  -- valFromLocal <- combineDyn (\a b -> if a == b then Nothing else a) (fmap Just localState) updateDyn
  -- let ev = updated $ nubDyn valFromLocal
  -- -- debug $ (("is eq: " ++) . show) <$> ev
  -- return $ fmapMaybe id ev
-- New target type signature: ctx -> dyn a -> m (dyn a, evnt a, eent hint)

sequencer ::MonadWidget t m => Dynamic t Context -> Map Int (String, [Bool]) -> Event t (Map Int (String,[Bool])) -> m (Dynamic t (Map Int (String,[Bool]), Event t (Map Int (String,[Bool])), Event t Hint))
sequencer ctx iMap update = elClass "table" "sequencer" $ mdo
  let seqLen = maybe 8 id $ maximumMay $ fmap (length . snd) $ elems iMap
  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]
  let downstreamEvs = leftmost [updateEvs, newRow, deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))

  -- update: newly added rows + updates for each button
  -- serverDeletes: rows to be deleted requested from server
  -- newRow: new row creation from clicking '+' locally
  -- deleteEvents: delete row from local '-'
  text "t2"
  sequencerIndicatorRow ctx seqLen
  widgets <- liftM joinDynThroughMap $ listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow) -- Dyn t (Map Int ((String,[Bool]), Event t ()))
  values <- mapDyn (fmap fst) widgets -- Dyn t (Map Int (String,[Bool]))
  dynEvs <- mapDyn (mergeMap . fmap (snd)) widgets -- Event t (Map Int ())
  let deleteEvents = fmap (Nothing <$) $ switch $ current $ dynEvs
  -- let updateVal = fmapMaybe id $ mergeWith (\a b->Nothing) [Nothing <$ update, Just <$> updated (nubDyn values)]
  updateVal <- blockServerUpdates update values
  debug updateVal
  debug $ ("coincidence happened") <$ coincidence (updated (nubDyn values) <$ update)
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) $ nubDyn values
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (Just ("",take seqLen $ repeat False))) (current maxKey) plusButton
  mapDyn (\v-> (v, updateVal, never)) values

--
-- sequencerRow' :: MonadWidget t m => Int -> (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((Int,String,[Bool], Event t ())))
-- sequencerRow' i iv ev = do
--   val <- sequencerRow' iv ev
--   return $ fmap (\(x,e)->(i,x,e)) val






-- Event returned is a message to delete that row
sequencerRow ::(MonadWidget t m) => (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((String,[Bool]), Event t ()))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  let strUpdate = fmap (T.pack . fst) edits
  let buttonUpdates = fmap (M.fromList . attachIndex . fmap Just . snd) edits
  let textInputAttrs = singleton "class" "sequencerTextInputTd"
  deleteMe <- elClass "td" "delete" $ button "-" -- clickableTdClass (constDyn " - ") (constDyn "delete") ()
  rowInput <- elClass "td" "sequencerTextInputTd" $ textInput $ def & textInputConfig_initialValue .~ (T.pack iVal) & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn empty)
  -- rowInput <- el "td" $ growingTextInput $ def & textInputConfig_initialValue .~ iVal & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn textInputAttrs)
  buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals buttonUpdates sequencerButton  -- Dyn (Map Int Bool)

  val <- combineDyn (\s b -> (T.unpack s, elems b)) (_textInput_value rowInput) buttons
  mapDyn (\x->(x,deleteMe)) val

sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t Bool)
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Mousedown) (return ())
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButton activated" else "sequencerButton") isActive
  return isActive















-- -- Event returned is a message to delete that row
-- sequencerRow ::(MonadWidget t m) => (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((String,[Bool]), Event t (String,[Bool]), Event t ()))
-- sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
--   let buttonIVals = M.fromList $ attachIndex vals
--   let strUpdate = fmap (T.pack . fst) edits
--   let buttonUpdates = fmap (M.fromList . attachIndex . fmap Just . snd) edits
--   let textInputAttrs = singleton "class" "sequencerTextInputTd"
--   deleteMe <- elClass "td" "delete" $ button "-" -- clickableTdClass (constDyn " - ") (constDyn "delete") ()
--   rowInput <- elClass "td" "sequencerTextInputTd" $ textInput $ def & textInputConfig_initialValue .~ (T.pack iVal) & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn empty)
--   -- rowInput <- el "td" $ growingTextInput $ def & textInputConfig_initialValue .~ iVal & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn textInputAttrs)
--   buttons <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals buttonUpdates sequencerButton  -- Dyn (Map Int Bool)
--   let buttons' = fmap (fmap fst . elems) buttons -- Dynamic t ([Bool])
--   let updateEvs = switchPromptlyDyn $ fmap (leftmost . fmap snd . elems) buttons -- Event t (Map Int Bool)
--   val <- combineDyn (\s b -> (T.unpack s, elems b)) (_textInput_value rowInput) buttons
--   mapDyn (\x->(x,deleteMe)) val
--
-- -- Dynamic t (Map Int (Dynamic t (Bool, Event t Bool)))
--
-- -- fmap (mapWithKey (\k (_,v) -> fmap (\x-> (k,x)) v)) buttons -- Dynamic t (Map Int (Event (Int,Bool)))
-- --
-- -- fmap (elems . mapWithKey (\k (_,v) -> fmap (\x-> (k,x)) v)) buttons -- Dynamic t ([Event (Int,Bool))])
-- --
-- -- switchPromptlyDyn $ fmap (leftmost . elems . mapWithKey (\k (_,v) -> fmap (\x-> (k,x)) v)) buttons -- (Event (Int,Bool))
-- --
-- --
-- -- Dynamic t (Map Int (Bool, Event t Bool))
--
--   -- listWithKeyShallowDiff :: (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
--
--
-- sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m Dynamic t (Bool, Event t Bool)
-- sequencerButton pos val edits = mdo
--   (element,_) <- elDynAttr' "td" attrs $ return ()
--   clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Mousedown) (return ())
--   let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
--   isActive <- holdDyn val $ leftmost [edits, clickUpdates]
--   attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButton activated" else "sequencerButton") isActive
--   return (fmap (\x-> (x,clickUpdates) isActive)









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
