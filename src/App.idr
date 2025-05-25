||| App模块提供了应用程序示例
||| 定义了示例API路由和处理函数
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

%runElab derive "User" [Show,Eq,ToJSON]

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
Api2 = "users" :> Capture "id" String :> "posts" :> Nil :-> Get (List String)
handler2 : HandlerType Api2
handler2 params = do
  let id = fromMaybe "" (lookup "id" params)
  let posts = ["post1", "post2", "post3"]

  pure $ (id ++ "--" ++ ) <$> posts

||| 服务器实例
||| 包含所有可用的API路由
public export
server : Server
server = MkServer [
  Api1 :=> handler1,
  Api2 :=> handler2
  ]