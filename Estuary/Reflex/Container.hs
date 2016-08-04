{-# LANGUAGE RecursiveDo #-}

module Estuary.Reflex.Container where

import Data.Map
import Reflex
import Reflex.Dom
import Data.Functor.Misc -- For Const2
import Control.Monad

data Construction a = Insert a | Replace a | Delete deriving (Show)

-- given a Map and a Construction operation at a specifed key, return the new map

applyConstruction :: (Num k, Ord k) => Map k a -> (k,Construction a) -> Map k a
applyConstruction m (k,Replace a) = insert k a m
applyConstruction m (k,Delete) = delete k m
applyConstruction m (k,Insert a) = insert k a m'
  where m' = mapKeys (f) m
        f x | member k m = if (x>=k) then (x + 1) else x
            | otherwise = x

-- given a Map and another map of Construction operations, return the resulting map

applyConstructionMap :: (Num k, Ord k) => Map k a -> Map k (Construction a) -> Map k a
applyConstructionMap oldMap cMap = foldlWithKey (\a k b -> applyConstruction a (k,b)) oldMap cMap


-- given a Map and another map of Construction operations, determine the complete
-- list of "construction" events as expected by the Reflex function listHoldWithKey

constructionDiff :: (Num k, Ord k, Eq v) => Map k v -> Map k (Construction v) -> Map k (Maybe v)
constructionDiff oldMap cMap = unions [deletions,additions,changes]
  where newMap = applyConstructionMap oldMap cMap
        deletions = fmap (const Nothing) $ difference oldMap newMap -- keys only in oldMap are deletions
        additions = fmap (Just) $ difference newMap oldMap -- keys only in newMap are additions
        changes = fmap (Just) $ intersection newMap $ Data.Map.filter (id) $ intersectionWith (/=) oldMap newMap


container :: (Ord k, Num k, Show k, Eq v, Show v, MonadWidget t m)
   => Map k v                                -- a map of initial values
   -> Event t (Map k (Construction v))       -- construction events (replace/insert/delete)
   -> Event t (Map k w)                      -- signaling events to be delivered to child widgets
   -> (v -> Event t w -> m (Dynamic t (v,Event t x)))                -- function to make a widget given initial value and signaling event
   -> m ( (Dynamic t (Map k v)) , Event t (Map k x) )

container initialValue cEvents rEvents mkChild = mdo
  let cEventsIn = traceEvent "cEventsIn" cEvents
  let existingMap' = traceDyn "existingMap'" $ values
  let cEvents' = traceEvent "cEvents'" $ attachDynWith (constructionDiff) existingMap' cEventsIn
  let selector = fanMap rEvents
  let mkChild' k v = mkChild v $ select selector $ (Const2 k)
  widgets <- liftM (joinDynThroughMap) $ listHoldWithKey initialValue cEvents' mkChild' -- Dynamic t (Map k (v,Event t x))
  values <- mapDyn (fmap (fst)) widgets
  events <- liftM (switchPromptlyDyn) $ mapDyn (mergeMap . fmap (snd)) widgets
  return (values,events)