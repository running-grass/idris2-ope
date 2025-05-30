module Pact.Server.Core

import Pact.API

import Data.Vect
import FS.Socket
import JSON.ToJSON

||| Route record type
||| Associates an API definition with its handler function
public export
record RouteItem (m : Type -> Type) where
  constructor (:=>)
  { paramsTypes : Vect paramsTypesLen Type}
  ||| API definition, describes path and endpoint
  routeApi: API paramsTypes
  ||| Handler function, type is determined by the API definition
  routeHandler : GetHandlerType m routeApi
  { auto toJSONProof : ToJSON (GetEpResultTypeFromAPI routeApi ) }


public export
GetEndpointTypeFromRouteItem : (m : Type -> Type) -> RouteItem m -> Type
GetEndpointTypeFromRouteItem m (api :=> handler) = GetEPFromAPI m api

||| Server data type
||| Contains a set of route definitions for handling HTTP requests
public export
data Router: (m : Type -> Type) -> Type where
  ||| Creates a server instance containing a list of routes
  MkRouter : (routes : List (RouteItem m)) -> Router m


||| ServerConfig is a record type that contains server configuration
||| 
||| Contains server bind address and max connection count
||| @ return ServerConfig
public export
record ServerConfig where
  constructor MkServerConfig
  ||| Server bind IP address and port
  bind : IP4Addr
  ||| Max allowed connections
  maxConns : Nat


||| defaultConfig is a function that returns the default server configuration
||| 
||| Binds to 127.0.0.1:2222, max connections 128
||| @ return ServerConfig
public export
defaultConfig : ServerConfig
defaultConfig = MkServerConfig  {
    bind = IP4 [127,0,0,1] 2222,
    maxConns = 128
  }
