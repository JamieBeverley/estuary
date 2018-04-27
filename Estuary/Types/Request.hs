module Estuary.Types.Request where

import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.Definition

type ServerRequest = Request Definition

data Request a =
  Authenticate String |
  GetEnsembleList |
  GetTutorialList  String | -- String denotes which kind of tutorial; "structure editing" vs. "text editing" etc..(which language)
  JoinEnsemble String |
  JoinTutorial String String |
  LeaveEnsemble |
  CreateEnsemble String String | -- ensembleName ensemblePassword (or "" for no password)
  CreateTutorial String String String | -- name pwd type
  EnsembleRequest (Sited String (EnsembleRequest a)) |
  GetServerClientCount
  deriving (Eq)

instance JSON a => JSON (Request a) where
  showJSON (Authenticate p) = encJSDict [("Authenticate",p)]
  showJSON (GetEnsembleList) = showJSON "GetEnsembleList"
  showJSON (GetTutorialList s) = encJSDict [("GetTutorialList",s)] -- Is that right? TODO
  showJSON (JoinEnsemble s) = encJSDict [("JoinEnsemble",s)]
  showJSON (JoinTutorial s t) = encJSDict [("JoinEnsemble",s),("type",showJSON t)]
  showJSON (LeaveEnsemble) = showJSON "LeaveEnsemble"
  showJSON (CreateEnsemble name pwd) = encJSDict [("CreateEnsemble",showJSON name),("pwd",showJSON pwd)]
  showJSON (CreateTutorial name pwd t) = encJSDict [("CreateTutorial",showJSON name),("pwd",showJSON pwd),("type",showJSON t)]
  showJSON (EnsembleRequest s) = encJSDict [("EnsembleRequest",showJSON s)]
  showJSON (GetServerClientCount) = showJSON "GetServerClientCount"
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj "Authenticate" x
  readJSON (JSString x) | fromJSString x == "GetEnsembleList" = Ok GetEnsembleList
  readJSON (JSObject x) | firstKey x == "GetTutorialList" = GetTutorialList <$> valFromObj "GetTutorialList" x -- TODO do I need the 'Ok' thing before?
  readJSON (JSObject x) | firstKey x == "JoinEnsemble" = JoinEnsemble <$> valFromObj "JoinEnsemble" x
  readJSON (JSObject x) | firstKey x == "JoinTutorial" = JoinTutorial <$> valFromObj "JoinTutorial" x <*> valFromObj "type" x
  readJSON (JSString x) | fromJSString x == "LeaveEnsemble" = Ok LeaveEnsemble
  readJSON (JSObject x) | firstKey x == "CreateEnsemble" = CreateEnsemble <$> valFromObj "CreateEnsemble" x <*> valFromObj "pwd" x
  readJSON (JSObject x) | firstKey x == "CreateTutorial" = CreateTutorial <$> valFromObj "CreateTutorial" x <*> valFromObj "pwd" x <*> valFromObj "type" x
  readJSON (JSObject x) | firstKey x == "EnsembleRequest" = EnsembleRequest <$> valFromObj "EnsembleRequest" x
  readJSON (JSString x) | fromJSString x == "GetServerClientCount" = Ok GetServerClientCount
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Request: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Request: " ++ (show x)
  readJSON _ = Error "Unable to parse as Request (neither JSOBject nor JSString)"
