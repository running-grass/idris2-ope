||| Define example API routes and handler functions
module Sample.Main1

import Pact.Server
import Pact.API.Experimental

import Data.SortedMap

import FS.Posix

import public IO.Async.Loop.Posix
import public IO.Async.Loop.Epoll

import Pact.WAI

import Data.String

import JSON.Derive

%language ElabReflection

record UserId where
  constructor MkUserId
  id : Nat 

implementation HasPathParam UserId where
  parsePathParams s = case parsePositive s of
    Just n => Just (MkUserId n)
    Nothing => Nothing

%runElab derive "UserId" [Show,Eq,ToJSON, FromJSON]

record User where
  constructor MkUser
  id : UserId
  name : String

%runElab derive "User" [Show,Eq,ToJSON, FromJSON]

p1 : Path () [()]
p1 = StaticPath "users"

p2 : Path UserId [UserId]
p2 = Capture "id" UserId

p3 : Path String [String, UserId, String]
p3 = Capture "users" String :/ p2 :/ Capture "name" String

p4 : Path () [(), UserId, String]
p4 = StaticPath "users" :/ p2 :/ Capture "name" String

ep1 : Endpoint () User
ep1 = Get User

-- api : API
Api2 = p3 :/ ep1

handler2 : GetHandlerType IO Api2
handler2 name userId pass = pure $ MkUser userId name

-- route2 : RouteItem IO
-- route2 = (Api2 :=> handler2) { toJSONProof = %search }

-- router : Router IO
-- router = MkRouter [ route2 ]



||| runServer is a function that runs the HTTP server
||| @ config Server configuration
||| @ server Server instance
||| @ return IO ()
covering
public export
runRouter: ServerConfig -> Router IO -> IO ()
runRouter config router = do
  let app = processRequest router
  runServer' $ serverFunc config app

||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  -- Create server config, set max connections to 1
  let config = { maxConns := 1 , bind :=  IP4 [0,0,0,0] 2223 } defaultConfig
  putStrLn "Starting server on the http://localhost:\{show config.bind.port}"
  -- Run the server with the config and application
  runRouter config router
