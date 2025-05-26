||| Define example API routes and handler functions
module Ope.Sample.Main

import Ope.Server
import Ope.API

import Data.SortedMap

import FS.Posix

import public IO.Async.Loop.Posix
import public IO.Async.Loop.Epoll

import Ope.WAI

import Data.String

import JSON.Derive

%language ElabReflection

||| Example user data type.
record User where
  constructor MkUser
  id: String
  name: String
  age: Int

%runElab derive "User" [Show,Eq,ToJSON, FromJSON]

map1 : Params
map1 = insert "id" "1" emptyParams

user1 : User
user1 = MkUser "1" "John" 20

||| Example GET API: get user by id.
Api1 : API
Api1 = "users" :> Capture "id" String :> Nil :-> Get User

||| Example GET API handler, returns a fixed user.
handler1 : HandlerType Api1
handler1 params = do
  let id = fromMaybe "" (lookup "id" params)
  pure $ user1

||| Example POST API: create a user by id and return a user list.
Api2 : API
Api2 = "users" :> Capture "id" String :> Nil :-> Post User (List User)

||| Example POST API handler, returns a list containing the new user.
handler2 : HandlerType Api2
handler2 params req = do
  let uid = fromMaybe "" (lookup "id" params)
  let user = { id := uid } req
  pure [user]

||| Server instance
||| Contains all available API routes
public export
server : Server
server = MkServer [
  Api1 :=> handler1,
  Api2 :=> handler2
  ]

||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  -- Create server config, set max connections to 1
  let config = { maxConns := 1 } defaultConfig
  putStrLn "Starting server on the http://localhost:\{show config.bind.port}"
  -- Run the server with the config and application
  runServer config server
