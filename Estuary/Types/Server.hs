module Estuary.Types.Server where

import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Sound.Tidal.Tempo

import Estuary.Types.Client
import Estuary.Types.Definition
import Estuary.Types.Sited
import Estuary.Types.Response
import Estuary.Types.View
import qualified Estuary.Types.Ensemble as E

data Server = Server {
  password :: String,
  clients :: Map.Map ClientHandle Client,
  ensembles :: Map.Map String E.Ensemble,
  tutorials:: Map.Map String E.Ensemble, -- Map TutorialType (Map TutorialName Tutorial)  - tutorials are just ensembles under the hood...
  connectionCount :: Int
}

newServer :: Server
newServer = Server {
  password = "",
  clients = Map.empty,
  ensembles = Map.empty,
  tutorials = Map.empty,
  connectionCount = 0
}


updateServer :: MVar Server -> (Server -> Server) -> IO (MVar Server)
updateServer s f = do
  s' <- takeMVar s
  putMVar s (f s')
  return s

updateClient :: MVar Server -> ClientHandle -> (Client -> Client) -> IO ()
updateClient s c f = do
  s' <- takeMVar s
  let c' = (clients s') Map.! c
  let c'' = f c'
  putMVar s $ s' { clients = Map.adjust (const c'') c (clients s') }

updateClientWithServer :: MVar Server -> ClientHandle -> (Server -> Client -> Client) -> IO ()
updateClientWithServer s c f = do
  s' <- takeMVar s
  let c' = (clients s') Map.! c
  let c'' = f s' c'
  putMVar s $ s' { clients = Map.adjust (const c'') c (clients s') }


getPassword :: MVar Server -> IO String
getPassword s = readMVar s >>= return . password

addClient :: Server -> WS.Connection -> (ClientHandle,Server)
addClient s x = (i,s { clients=newMap})
  where i = head ([0..] \\ Map.keys (clients s))
        newMap = Map.insert i (newClient i x) (clients s)

deleteClient :: ClientHandle -> Server -> Server
deleteClient h s = s { clients = Map.delete h (clients s) }

createEnsemble :: String -> String -> UTCTime -> Server -> Server
createEnsemble name pwd t s = s { ensembles = Map.insertWith (\_ x -> x) name e (ensembles s) }
  where e = E.setPassword pwd (E.emptyEnsemble t)

createTutorial :: String -> String -> UTCTime -> Server -> Server
createTutorial name pwd t s = s { tutorials = Map.insertWith (\_ x -> x) name e (tutorials s) }
  where e = E.setPassword pwd (E.emptyEnsemble t)

edit :: String -> Int -> Definition -> Server -> Server
edit w z d s = s { ensembles = Map.adjust (E.editDef z d) w (ensembles s) }

setView :: String -> String -> View -> Server -> Server
setView w k v s = s { ensembles = Map.adjust (E.editView k v) w (ensembles s) }

setDefaultView :: String -> View -> Server -> Server
setDefaultView w v s = s { ensembles = Map.adjust (E.editDefaultView v) w (ensembles s)}

deleteView :: String -> String -> Server -> Server
deleteView e v s = s { ensembles = Map.adjust (E.deleteView v) e (ensembles s) }

getEnsembleList :: MVar Server -> IO ServerResponse
getEnsembleList s = readMVar s >>= return . EnsembleList . Map.keys . ensembles

getViews :: MVar Server -> String -> IO [String]
getViews s w = readMVar s >>= return . fromMaybe [] . fmap (Map.keys . E.views) . Map.lookup w . ensembles

getView :: MVar Server -> String -> String -> IO (Maybe View)
getView s e v = do
  s' <- readMVar s
  return $ do
    e' <- Map.lookup e (ensembles s')
    Map.lookup v (E.views e')

getEnsemblePassword :: MVar Server -> String -> IO String
getEnsemblePassword s e = readMVar s >>= return . fromMaybe [] . fmap (E.password) . Map.lookup e . ensembles

tempoChangeInEnsemble :: String -> UTCTime -> Double -> Server -> Server
tempoChangeInEnsemble e time newCps s = s { ensembles = Map.adjust (E.tempoChange time newCps) e (ensembles s) }

getTempoInEnsemble :: MVar Server -> String -> IO (Maybe Tempo)
getTempoInEnsemble s e = readMVar s >>= return . fmap E.tempo . Map.lookup e . ensembles

editTutorial :: String -> Int -> Definition -> Server -> Server
editTutorial w z d s = s { tutorials = Map.adjust (E.editDef z d) w (tutorials s) }


setTutorialView :: String -> String -> View -> Server -> Server
setTutorialView w k v s = s { tutorials = Map.adjust (E.editView k v) w (tutorials s) }

setDefaultTutorialView :: String -> View -> Server -> Server
setDefaultTutorialView w v s = s { tutorials = Map.adjust (E.editDefaultView v) w (tutorials s)}

deleteTutorialView :: String -> String -> Server -> Server
deleteTutorialView e v s = s { tutorials = Map.adjust (E.deleteView v) e (tutorials s) }

getTutorialList :: MVar Server  -> IO ServerResponse  -- String indicating what type of  tutorial  (Tidalcycles vs cQuence, etc...)
getTutorialList s  = readMVar s >>= return . TutorialList . Map.keys  . tutorials

getTutorialViews :: MVar Server -> String -> IO [String]
getTutorialViews s w = readMVar s >>= return . fromMaybe [] . fmap (Map.keys . E.views) . Map.lookup w . tutorials

getTutorialView :: MVar Server -> String -> String -> IO (Maybe View)
getTutorialView s e v = do
  s' <- readMVar s
  return $ do
    e' <- Map.lookup e (tutorials s')
    Map.lookup v (E.views e')

tempoChangeInTutorial :: String -> UTCTime -> Double -> Server -> Server
tempoChangeInTutorial e time newCps s = s { tutorials = Map.adjust (E.tempoChange time newCps) e (tutorials s) }

getTempoInTutorial :: MVar Server -> String -> IO (Maybe Tempo)
getTempoInTutorial s e = readMVar s >>= return . fmap E.tempo . Map.lookup e . tutorials


getTutorialPassword :: MVar Server -> String -> IO String
getTutorialPassword s e = readMVar s >>= return .fromMaybe [] . fmap (E.password) . Map.lookup e . tutorials


getServerClientCount :: MVar Server -> IO Int
getServerClientCount s = readMVar s >>= return . Map.size . clients
