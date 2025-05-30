||| Define example API routes and handler functions
module Sample.Main

import Pact.Server

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

api1 = (StaticPath "users" :/ Capture "id" UserId :/ Capture "name" String) :> Get JSONAccept User

handler1 : UserId -> String -> IO User
handler1 userId name = pure $ MkUser userId name

router : Router IO
router = MkRouter [ api1 :=> handler1 ]


||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  -- Create server config, set max connections to 1
  let config = { maxConns := 1 , bind :=  IP4 [0,0,0,0] 2222 } defaultConfig
  putStrLn "Starting server on the http://localhost:\{show config.bind.port}"
  -- Run the server with the config and application
  runRouter config router
