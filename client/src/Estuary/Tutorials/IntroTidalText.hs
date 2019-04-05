{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Estuary.Tutorials.IntroTidalText where

import Reflex
import Reflex.Dom

import qualified Data.IntMap.Strict as IM
import Data.Map as M
import Estuary.Tutorials.Tutorial
import Estuary.Types.Language

import Estuary.Types.Definition

import Control.Monad (liftM)
import qualified Data.Text as T

-- miniTidalWidget
import Estuary.Types.Context
import Estuary.Types.Hint


introTidalText ::MonadWidget t m => Tutorial t m
introTidalText = Tutorial IntroTidalText introTidalTextWidget


introTidalTextPages::Map Int (Map Language T.Text, Map Language T.Text, T.Text)
introTidalTextPages = fromList $ Prelude.zip [0..] [p1,p2]

p1 = (
  M.fromList [(English,"Title p1")],
  M.fromList [(English,"instruction text instruction text instructiontext instruction text instruction text instructiontext instruction text instruction text instructiontext instruction text instruction text instructiontext")],
  "s \"bd\""
  )

p2 = (
  M.fromList [(English,"Title p2")],
  M.fromList [(English,"222222222instruction text instruction text instructiontext instruction text instruction text instructiontext instruction text instruction text instructiontext instruction text instruction text instructiontext")],
  "s \"bd dr\""
  )


introTidalTextWidget:: MonadWidget t m => Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
introTidalTextWidget ctx = elClass "div" "tutorial" $ mdo
  let emptyPage = (M.empty, M.empty, "")
  widget <- widgetHold (page ctx (maybe emptyPage id $ M.lookup 0 introTidalTextPages)) changePageEv
  let val =  joinDyn $ fmap fst widget
  let hint = switchPromptlyDyn $ fmap snd widget
-- m (Dynamic t (Dynamit t Definition Map, Event t Hint))
  -- widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
  let numPages = length $ keys introTidalTextPages
  currentPage <- foldDyn (\a b-> min (max 0 (b+a)) (numPages-1) ) 0 $ leftmost [back, forward]
  let changePageEv = fmap (page ctx . maybe emptyPage id . ((flip M.lookup) introTidalTextPages) ) $ updated currentPage
  back <- liftM (fmap (const (-1))) $ button "back"
  forward <- liftM (fmap (const 1)) $ button "forward"
  dynText $ fmap (T.pack . (++"/"++ show numPages) . show . (+1)) currentPage
  return (val,hint)

page:: MonadWidget t m => Dynamic t Context -> (Map Language T.Text, Map Language T.Text, T.Text) -> m (Dynamic t DefinitionMap, Event t Hint)
page ctx (titleText, descriptionText, val) = do
  title ctx titleText
  labelWidget ctx descriptionText
  miniTidalWidget ctx 3 (T.unpack val)
