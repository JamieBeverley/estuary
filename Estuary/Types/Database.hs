{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import Data.Map
import Data.Time.Clock
import Text.JSON
import Data.Text

import Estuary.Types.View
import Estuary.Types.Ensemble

openDatabase :: IO Connection
openDatabase = do
  c <- open "Estuary.db"
  createLogTable c
  createEnsembleTable c
  createTutorialTable c
  postLogToDatabase c "database opened"
  return c

createEnsembleTable :: Connection -> IO ()
createEnsembleTable c = execute_ c "CREATE TABLE IF NOT EXISTS ensembles (name TEXT, json TEXT, lastUpdate TEXT)"

createTutorialTable :: Connection -> IO ()
createTutorialTable c = execute_ c "CREATE TABLE IF NOT EXISTS tutorials (name TEXT, json TEXT, type TEXT, lastUpdate TEXT)"

createLogTable :: Connection -> IO ()
createLogTable c = execute_ c "CREATE TABLE IF NOT EXISTS log (time TEXT,msg TEXT)"

postLogToDatabase :: Connection -> String -> IO ()
postLogToDatabase c l = do
  now <- getCurrentTime
  execute c "INSERT INTO log (time,msg) VALUES (?,?)" (now,l)

writeNewEnsemble :: Connection -> String -> Ensemble -> IO ()
writeNewEnsemble c eName e = do
  now <- getCurrentTime
  execute c "INSERT INTO ensembles (name,json,lastUpdate) VALUES (?,?,?)" (eName,e,now)

writeEnsemble :: Connection -> String -> Ensemble -> IO ()
writeEnsemble c eName e = do
  now <- getCurrentTime
  execute c "UPDATE ensembles SET json=?, lastUpdate=? WHERE name=?" (e,now,eName)

writeNewTutorial :: Connection -> String -> String -> Ensemble -> IO ()
writeNewTutorial c tName tType t = do
  now <- getCurrentTime
  execute c "INSERT INTO tutorials (name, json, type, lastUpdate) VALUES (?,?,?,?)" (tName,t,tType,now)

writeTutorial::Connection -> String -> String -> Ensemble -> IO ()
writeTutorial c tName tType t = do
  now <- getCurrentTime
  execute c "UPDATE tutorials SET json=?, lastUpdate=? WHERE name=? AND type=?" (t,now,tName,tType)

instance ToField Ensemble where
  toField = SQLText . pack . encode

instance FromField Ensemble where
  fromField = f . decode . g . fieldData
    where g (SQLText t) = unpack t
          g _ = ""
          f (Text.JSON.Ok x) = Database.SQLite.Simple.Ok.Ok x
          f (Text.JSON.Error x) = error x

readEnsembles:: Connection -> IO (Map String Ensemble)
readEnsembles c = do
  r <- query_ c "SELECT name,json FROM ensembles"
  return $ fromList r

readTutorials :: Connection -> IO (Map String (Map String Ensemble))
readTutorials c = do
  r <- query_ c "SELECT name,json,type FROM tutorials" -- [(n,j)]
  let tutorials =  Prelude.foldl (\b (tN,t,tt) -> insertWith union tt (Data.Map.singleton tN t)  b  ) Data.Map.empty r   -- TODO make more predictable for when tutorial type and name are the same... (currently the first occurence stays (I think) b.c. of how union works)
  return  tutorials


closeDatabase :: Connection -> IO ()
closeDatabase = close
