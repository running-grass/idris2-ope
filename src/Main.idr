||| The Main module is the entry point for the HTTP server
||| Responsible for configuring and starting the server
module Main

import Ope.Server.Type
import Ope.Server
import App
import Ope.API

||| HTTP server application entry
||| Handles all HTTP requests and routes them to the corresponding handler
||| @ req HTTP request object
public export
app : Request -> HTTPStream Response
app req = processRequest server req

||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  -- Create server config, set max connections to 1
  let config = { maxConns := 1 } defaultConfig
  putStrLn "Starting server on the http://localhost:{show config.bind.port}"
  -- Run the server with the config and application
  runServer $ server config app
