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

||| 用户API示例
||| 定义了获取用户数量的GET路由
Api1 : API
Api1 = StaticPath "users" :-> Get Nat

||| 文章列表API示例
||| 定义了获取文章列表的GET路由
Api2 : API
Api2 = StaticPath "posts" :> StaticPath "list" :-> Get String

||| 用户API处理函数
||| 返回固定数值42作为示例
handler1 : HandlerType Api1
handler1 params = pure 42

||| 文章列表API处理函数
||| 返回固定字符串"hello"作为示例
handler2 : HandlerType Api2
handler2 params = pure "hello"

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

||| HTTP 服务器应用入口
||| 处理所有HTTP请求，将请求路由到对应的处理函数
||| @ req HTTP请求对象
public export
app : Application
app req = processRequest server req