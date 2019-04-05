{-# LANGUAGE RecursiveDo, GADTs, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Estuary.Tutorials.Tutorial where

import Control.Monad (liftM)

import qualified Data.IntMap.Strict as IM
import Data.Map as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.RenderInfo
import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.Hint
import Estuary.Types.Language
import Estuary.Types.Request
import Estuary.Types.View
import Estuary.Widgets.Generic
import Estuary.Widgets.View
import Estuary.Widgets.Text (textNotationWidget)
import Estuary.Types.TidalParser
import Estuary.Types.TextNotation
import GHC.Generics

import GHCJS.Marshal

import Estuary.Widgets.Generic (debug)

import Reflex
import Reflex.Dom hiding (getKeyEvent, preventDefault)
import GHCJS.DOM.EventM
import Reflex.Dom.Contrib.KeyEvent


import Estuary.Types.Language
import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Widgets.Generic (clickableDivAttrs)
import Estuary.Reflex.Utility (buttonDynAttrs)

-- ! Don't forget to add show instance when adding new tutorial
data TutorialId = IntroTidalText deriving (Eq, Ord, Generic, FromJSVal, ToJSVal)

instance Show TutorialId where
  show IntroTidalText = "A Brief Introduction to TidalCycles (MiniTidal)"
  show _ = "<tutorial>"

data Tutorial t m = Tutorial {
  tutorialId::TutorialId,
  widget::(Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint))
}


attachIndex :: [a] -> [(Int,a)]
attachIndex = zip [0..]

dynList::MonadWidget t m => [Dynamic t a] -> m (Dynamic t [a])
dynList l = return $ fmap elems $ joinDynThroughMap $ constDyn $ fromList $ attachIndex l

title::MonadWidget t m => Dynamic t Context -> Map Language Text -> m ()
title ctx langMap = elClass "div" "title" $ labelWidget ctx langMap

labelWidget::MonadWidget t m => Dynamic t Context -> M.Map Language Text -> m ()
labelWidget ctx txt = do
  let dflt = safeHead "" $ elems txt
  let str = fmap (\c-> maybe dflt id $ M.lookup (language c) txt) ctx
  dynText str
  where
    safeHead a [] = a
    safeHead _ (x:xs) = x

miniTidalWidget :: MonadWidget t m => Dynamic t Context -> Int -> String -> m (Dynamic t DefinitionMap, Event t Hint)
miniTidalWidget ctx rows initialText = elClass "div" "panel" $ do
  let silent = (Live (TidalTextNotation MiniTidal,"") L3)
  shh <- buttonDynAttrs "silence" silent (constDyn $ M.fromList [("style","float:right")])
  eval <- buttonDynAttrs "eval" () (constDyn $ M.fromList [("style","float:left")])
  x <- textArea $ def & textAreaConfig_attributes .~ (constDyn $ M.fromList [("rows", T.pack $ show rows)]) & textAreaConfig_initialValue .~ (T.pack initialText)
  let e = _textArea_element x
  e' <- wrapDomEvent (e) (onEventName Keypress) $ do
    y <- getKeyEvent
    if (keyPressWasShiftEnter y) then (preventDefault >> return True) else return False
  let evalVal = tagPromptlyDyn (fmap ((\str -> Live (TidalTextNotation MiniTidal,str) L3) . T.unpack) $ _textArea_value x) $ leftmost [eval, () <$ ffilter id e']
  val <- holdDyn (Live (TidalTextNotation MiniTidal, "") L3) $  leftmost [evalVal, shh]
  let defn = fmap (\v-> IM.singleton 1 (TextProgram v)) val
  return (defn, never)
  where keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13)







--
