||| Mock模块提供了交互式编程环境的测试数据
module Mock

import Ope.Server.Type
import Ope.API

import Data.SortedMap

import FS.Posix

import public IO.Async.Loop.Posix
import public IO.Async.Loop.Epoll

import Ope.WAI

||| 用户API示例
||| 定义了获取用户数量的GET路由
Api1 : API
Api1 = Static "users" :> Get Nat

||| 文章列表API示例
||| 定义了获取文章列表的GET路由
Api2 : API
Api2 = Static "posts" :> Static "list" :> Get String


Api3 : API
Api3 = Static "users" :> Capture "id" Nat :> Get String

Api4 : API
Api4 = Static "users" :> Static "info" :> Capture "id" Nat :> Static "name" :> Static "age" :> Capture "name" String :> Get String

Api5 : API
Api5 = Static "info" :> Capture "id" Nat :> Static "age" :> Get String

||| 用户API处理函数
||| 返回固定数值42作为示例
handler1 : APIHandlerType' Api1
handler1 = 42

||| 文章列表API处理函数
||| 返回固定字符串"hello"作为示例
handler2 : APIHandlerType' Api2
handler2 = "hello"

handler3 : APIHandlerType' Api3
handler3 id = "hello " ++ show id ++ "!"

||| 用户API路由定义
||| 将API定义和处理函数关联起来
route1 : Route
route1 = MkRoute Api1 handler1

||| 文章列表API路由定义
||| 将API定义和处理函数关联起来
route2 : Route
route2 = MkRoute Api2 handler2

||| 服务器实例
||| 包含所有可用的API路由
public export
server : Server
server = MkServer [route1, route2]

||| 用户API测试请求
req1 : Request
req1 = MkRequest GET "/users" (SortedMap.fromList []) V10 (SortedMap.fromList []) 0 Nothing (pure ())

||| 用户详情API测试请求
req2 : Request
req2 = MkRequest GET "/users/123" (SortedMap.fromList []) V10 (SortedMap.fromList []) 0 Nothing (pure ())

||| 文章API测试请求
req3 : Request
req3 = MkRequest GET "/posts" (SortedMap.fromList []) V10 (SortedMap.fromList []) 0 Nothing (pure ())

||| 文章列表API测试请求
req4 : Request
req4 = MkRequest GET "/posts/list" (SortedMap.fromList []) V10 (SortedMap.fromList []) 0 Nothing (pure ())
