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

import Data.Time
import Control.Monad.IO.Class (liftIO)

type Sequence a = Map Int (Maybe a,[Bool])

attachIndex:: [a] -> [(Int,a)]
attachIndex l = zip (take (length l) [0..]) l

showTempo::Tempo->String
showTempo (Tempo cp a b) = "cps: "++ show cp ++"  at: "++show a++ "  beat: "++ show b
-- beat: cycle count
-- at: utc time
-- cps - duh



sequencer ::MonadWidget t m => Dynamic t Context -> Map Int (String, [Bool]) -> Event t (Map Int (String,[Bool])) -> m (Dynamic t (Map Int (String,[Bool]), Event t (Map Int (String,[Bool])), Event t Hint))
sequencer ctx iMap update = elClass "table" "sequencer" $ mdo
  let seqLen = maybe 8 id $ maximumMay $ fmap (length . snd) $ elems iMap
  let serverDeletes = fmap (Nothing <$) $ attachWith M.difference (current values) update -- for deleted rows
  let updateEvs = mergeWith union [fmap (fmap Just) update, serverDeletes]
  let downstreamEvs = leftmost [updateEvs, newRow, deleteEvents] -- Event t (Map Int (Maybe (String,[Bool])))

  tmp <- liftM nubDyn $ mapDyn tempo ctx
  iTempo <- liftM (tagDyn tmp) getPostBuild
  debug $ (("tempo control update: " ++) . show) <$> leftmost [updated tmp, iTempo]
  let tempoWidgetUpdate = (\a-> sequencerBeatCount (cps a) (beat a) seqLen ) <$> leftmost [updated tmp, iTempo] -- Event t (m (Event t Int))
  beatEv <- liftM switchPromptlyDyn $ widgetHold (return never) $ tempoWidgetUpdate
  b1 <- mapDyn (beat . tempo) ctx
  debug $ ((++) "beat on beat-track update: " . show) <$> tagDyn b1 beatEv
  sequencerBeatRow seqLen beatEv

  widgets <- liftM joinDynThroughMap $ listWithKeyShallowDiff iMap downstreamEvs (const sequencerRow) -- Dyn t (Map Int ((String,[Bool]), Event t ()))
  values <- mapDyn (fmap fst) widgets -- Dyn t (Map Int (String,[Bool]))
  dynEvs <- mapDyn (mergeMap . fmap (snd)) widgets -- Event t (Map Int ())
  let deleteEvents = fmap (Nothing <$) $ switch $ current $ dynEvs
  let updateVal = fmapMaybe id $ mergeWith (\a b->Nothing) [Nothing <$ update, fmap Just $ updated (nubDyn values)]
  maxKey <- mapDyn (maybe 0 id . maximumMay . keys) $ nubDyn values
  plusButton <- el "tr" $ clickableTdClass (constDyn " + ") (constDyn "") ()
  let newRow = attachWith (\k _-> singleton (k+1) (Just ("",take seqLen $ repeat False))) (current maxKey) plusButton
  mapDyn (\v-> (v, updateVal, never)) values


-- Event returned is a message to delete that row
sequencerBeatRow:: MonadWidget t m => Int -> Event t Int -> m ()
-- sequencerBeatRow (iVal,vals) edits = elClass "tr" "sequencerRow" $ do
sequencerBeatRow seqLen activate = elClass "tr" "sequencerRow" $ do
  -- deleteMe <- elClass "td" "delete" $ button "-" -- clickableTdClass (constDyn " - ") (constDyn "delete") ()
  -- rowInput <- elClass "td" "sequencerTextInputTd" $ textInput $ def & textInputConfig_attributes .~ (constDyn empty)
  elClass "td" "sequencerIndicatorSpacer" $ return ()
  classes <- holdDyn (take seqLen $ repeat "sequencerIndicator") $ fmap (\a -> (take a $ repeat "sequencerIndicator")++"sequencerIndicatorActivated":(take (seqLen-1-a) $ repeat "sequencerIndicator")) activate -- Dyn t [String]
  classMap <- mapDyn (fromList . zip [0::Int, 1..]) classes -- Dyn map Int STring
  listWithKey classMap sequenceIndicator
  return ()




sequencerBeatRow':: MonadWidget t m => Int -> Event t Int -> m ()
sequencerBeatRow' seqLen activate = elClass "tr" "sequencerRow" $ do
  classes <- holdDyn (take seqLen $ repeat "sequencerIndicator") $ fmap (\a -> (take (a-1) $ repeat "sequencerIndicator")++"sequencerIndicatorActivated":(take (seqLen-1-a) $ repeat "sequencerIndicator")) activate -- Dyn t [String]
  classMap <- mapDyn (fromList . zip [0::Int, 1..]) classes -- Dyn map Int STring

  -- sequence ()
  -- let imap = fromList $ zip [0::Int,1..] $ repeat ""
  elClass "td" "sequencerIndicator1" $ return ()
  elClass "td" "sequencerIndicator2" $ return ()
  listWithKey classMap sequenceIndicator

-- listWithKey :: forall t k v m a. (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))

  -- listWithKeyShallowDiff imap (((Just <$>) . fromList . zip [0::Int, 1..]) <$> updated classes) (const sequenceIndicator)
  -- listWithKeyShallowDiff :: (Ord k, MonadWidget t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
  -- debug $ updated classes
  return ()

sequenceIndicator:: MonadWidget t m => Int -> Dynamic t String -> m ()
sequenceIndicator _ c = do
  attrs <- mapDyn (singleton "class") c
  elDynAttr "td" attrs $ return ()

sequenceIndicator' :: MonadWidget t m => String -> Event t String -> m ()
sequenceIndicator' v ev = do
  attrs <- holdDyn (fromList [("class",v)]) $ fmap (\x-> fromList [("class",x)]) ev
  elDynAttr "td" attrs $ return ()
  return ()

sequencerBeatCount ::(MonadWidget t m) => Double-> Double -> Int -> m (Event t Int)
sequencerBeatCount c b seqLen = do
  now <- liftIO getCurrentTime
  tick <- tickLossy ((1.0::NominalDiffTime)/(fromIntegral seqLen)/(realToFrac c)) now -- Event t TickInfo
  intTick <- foldDyn (\x y-> mod (y+1) seqLen) 0 tick
  let beatDuration = 1/c/(fromIntegral seqLen)
  let fractionalDiff = realToFrac $ (1-(b-(fromIntegral ((floor b)::Int))))*beatDuration
  liftIO $ putStrLn $ ("fractionalDiff: "++) $  show fractionalDiff
  -- let integerDiff = realToFrac $ 0
  delay (fractionalDiff) $ updated intTick












-- cps, beat, seqlen -> event int
sequencerBeatCount' ::(MonadWidget t m) => Double-> Double -> Int -> m (Event t Int)
sequencerBeatCount' c b seqLen = do
  now <- liftIO getCurrentTime
  tick <- tickLossy ((1.0::NominalDiffTime)/(fromIntegral seqLen)/(realToFrac c)) now -- Event t TickInfo
  v <- foldDyn (\x y-> mod (y+1) seqLen) 0 tick
  let beatDuration = 1/c/(fromIntegral seqLen)
  -- delay ((realToFrac (b - (fromIntegral $ floor b)))*beatDuration + (realToFrac $ (fromIntegral floor b) * beatDuration)) $ updated v
  let x1 = fromIntegral $ mod (floor b) seqLen -- uhg...
  let x2 = realToFrac $ x1*beatDuration   --
  liftIO $ putStrLn ("beat offet: " ++ show x2)
  let x3 = realToFrac $ (b - (fromIntegral ((floor b)::Int))) * beatDuration
  pb <- getPostBuild
  debug $ fmap (\_-> ("delay: " ++ (show $ x2 + x3))) pb
  delay (x2 + x3) $ updated v
  -- delay ((b - (realToFrac $ fromIntegral $ floor b))*beatDuration + (floor b * beatDuration)) $ updated v
  -- debug $ updated v
  -- return $ updated v

-- 42.29

-- 42*(1/0.45/8)

-- 0.5

-- generateEventEvery:: MonadWidget t m => Double -> m (Event t ())
-- generateEventEvery sec = do
--
--   tickLossy :: MonadWidget t m => NominalDiffTime -> UTCTime -> m (Event t TickInfo)




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
