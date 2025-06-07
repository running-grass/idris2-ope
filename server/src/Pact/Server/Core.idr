||| Define the core of the server.
module Pact.Server.Core

import Pact.API
import Pact.WAI.HTTPErr

import Data.Vect
import FS.Socket
import JSON.FromJSON
import Control.Monad.Either
import Control.Monad.Reader

||| Handler is a type that represents a handler function
public export
Handler : Type -> Type
Handler = EitherT HTTPErr IO

-- 在 Core.idr 中添加
public export
interface Hoistable (m : Type -> Type) where
  hoist : m a -> Handler a

-- 基本实现
public export
implementation Hoistable IO where
  hoist = liftIO

public export
implementation Hoistable Handler where
  hoist = id

||| Route record type
||| Associates an API definition with its handler function
public export
record RouteItem (m : Type -> Type) where
  constructor (:=>)
  ||| API definition, describes path and endpoint
  routeApi: API
  ||| Handler function, type is determined by the API definition
  routeHandler : GetHandlerType m routeApi
  { auto mimeRenderProof : MimeRender (VerbAccept routeApi.verb) (VerbResponse routeApi.verb) }
  { auto reqBodyProof : FromJSON (ApiReqBody routeApi)}


public export
GetEndpointTypeFromRouteItem : (m : Type -> Type) -> RouteItem m -> Type
GetEndpointTypeFromRouteItem m (api :=> handler) = GetEPFromAPI m api

||| Server data type
||| Contains a set of route definitions for handling HTTP requests
public export
data Router: (m : Type -> Type) -> Type where
  ||| Creates a server instance containing a list of routes
  MkRouter : Hoistable m => (routes : List (RouteItem m)) -> Router m


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
