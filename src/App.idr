||| The App module provides application examples
||| Defines example API routes and handler functions
module App

import Ope.Server.Type
import Ope.API

import Data.SortedMap

import FS.Posix

import public IO.Async.Loop.Posix
import public IO.Async.Loop.Epoll

import Ope.WAI

import Data.String

import JSON.Derive

%language ElabReflection


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

Api1 : API
Api1 = "users" :> Capture "id" String :> Nil :-> Get User

handler1 : HandlerType Api1
handler1 params = do
  let id = fromMaybe "" (lookup "id" params)
  pure $ user1

Api2 : API
Api2 = "users" :> Capture "id" String :> Nil :-> Post User (List User)

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