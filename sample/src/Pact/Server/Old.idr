module Pact.Server.Old

import Pact.Server
import Pact.API.Old
import Pact.WAI


||| runServer is a function that runs the HTTP server
||| @ config Server configuration
||| @ server Server instance
||| @ return IO ()
covering
public export
runServer : ServerConfig -> Server -> IO ()
runServer config server = do
  let app = processRequest server
  runServer' $ serverFunc config app
