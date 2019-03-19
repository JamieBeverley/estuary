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


sequencer ::MonadWidget t m => Dynamic t Context -> Map Int (String, [Bool]) -> Event t (Map Int (String,[Bool])) -> m (Dynamic t (Map Int (String,[Bool]), Event t (Map Int (String,[Bool])), Event t Hint))
sequencer ctx iMap update = elClass "table" "sequencer" $ mdo
  let seqLen = maybe 8 id $ maximumMay $ fmap (length . snd) $ elems iMap
  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]
  let downstreamEvs = leftmost [updateEvs, fmap (Just <$>) newRow, fmap (Nothing <$) deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))
  sequencerIndicatorRow ctx seqLen
  widgets <- liftM joinDynThroughMap $ listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow) -- Dyn t (Map Int ((String,[Bool]), Event t ()))
  let values = fmap (fmap (\(x,_,_)-> x)) widgets -- Dyn t (Map Int (String,[Bool]))
  let deleteEvents = switch $ current $ fmap (mergeMap . fmap (\(_,_,d)->d)) widgets -- Event t (Map Int ())
  let updateChildren = switch $ current $  fmap (mergeMap . fmap (\(_,x,_)->x)) widgets -- Event t (Map Int (String,[Bool]))
  let updateVal = leftmost [attachDynWith (flip M.union) values updateChildren, attachDynWith (M.difference) values deleteEvents, attachDynWith (flip M.union) values newRow]
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) $ nubDyn values
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (("",take seqLen $ repeat False))) (current maxKey) plusButton
  mapDyn (\v-> (v, updateVal, never)) values


-- Event returned is a message to delete that row
sequencerRow ::(MonadWidget t m) => (String,[Bool]) -> Event t (String,[Bool]) -> m (Dynamic t ((String,[Bool]), Event t (String,[Bool]), Event t ()))
sequencerRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
  let buttonIVals = M.fromList $ attachIndex vals
  let strUpdate = fmap (T.pack . fst) edits
  let buttonUpdates = fmap (M.fromList . attachIndex . fmap Just . snd) edits
  let textInputAttrs = singleton "class" "sequencerTextInputTd"
  deleteMe <- elClass "td" "delete" $ button "-" -- clickableTdClass (constDyn " - ") (constDyn "delete") ()
  rowInput <- elClass "td" "sequencerTextInputTd" $ textInput $ def & textInputConfig_initialValue .~ (T.pack iVal) & textInputConfig_setValue .~ strUpdate & textInputConfig_attributes .~ (constDyn empty)
  widgets <-  liftM joinDynThroughMap $ listWithKeyShallowDiff buttonIVals buttonUpdates sequencerButton  -- Dyn (Map Int (Bool, Event Bool))
  let buttons = fmap (elems . fmap fst) widgets -- Dyn (Map [Bool])
  let buttonEdits = switchPromptlyDyn $ fmap (mergeMap . fmap snd) widgets -- Event (Map Int Bool)
  let buttonEdits' = tagDyn buttons buttonEdits
  let strEdits = fmap T.unpack $ _textInput_input rowInput
  let updates = leftmost [attachPromptlyDyn (fmap (T.unpack) $ _textInput_value rowInput) buttonEdits', attachPromptlyDynWith (flip (,)) (buttons) strEdits]

  val <- combineDyn (\s b -> (T.unpack s, b)) (_textInput_value rowInput) buttons
  mapDyn (\x->(x, updates,deleteMe)) val

sequencerButton::(MonadWidget t m) => Int -> Bool -> Event t Bool -> m (Dynamic t (Bool, Event t Bool))
sequencerButton pos val edits = mdo
  (element,_) <- elDynAttr' "td" attrs $ return ()
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Mousedown) (return ())
  let clickUpdates = attachWith (\v _-> not v) (current isActive) $ leftmost [clickEv]
  isActive <- holdDyn val $ leftmost [edits, clickUpdates]
  attrs <- mapDyn (\x-> singleton "class" $ if x then "sequencerButton activated" else "sequencerButton") isActive
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
