||| API模块实现了路由匹配和请求处理的核心功能
||| 提供了将HTTP请求与类型安全API定义匹配的机制
module Ope.API

import Ope.Server.Type
import Data.String
import Data.List1
import Data.SortedMap
import public Ope.API.Core

import Ope.WAI
import JSON.ToJSON

fillDefault: Lazy a -> Maybe a -> Maybe a
fillDefault def Nothing = Just def
fillDefault _ (Just a) = Just a

||| 路径匹配函数
||| 检查请求路径段是否匹配 API 路径定义
||| 如果匹配成功，返回提取的路径参数
||| @ segments 请求的路径段列表
||| @ path API路径定义
public export
matchPath : Maybe Params -> (segments : List String) -> (path : Path) -> Maybe Params
matchPath Nothing [] _ = Nothing
matchPath prevParams [s] (StaticPath path :> Nil) = 
    if s == path then (fillDefault emptyParams prevParams) else Nothing
matchPath prevParams [s] (Capture key ct :> Nil) = insert key s <$> fillDefault emptyParams prevParams
-- 如果有多个路径段，只能匹配组合路径
matchPath prevParams (s :: segments) (path :> rest) = 
  case matchPath prevParams [s] (path :> Nil) of
    Just params => matchPath (Just params) segments rest
    Nothing => Nothing
matchPath _ _ _ = Nothing

||| API 匹配函数
||| 检查请求路径段是否匹配 API 定义
||| 内部调用 matchPath 处理具体的路径匹配
||| @ segments 请求的路径段列表
||| @ api API定义
public export
matchAPI : List String -> API -> Maybe Params
matchAPI segments (path :-> _) = matchPath Nothing segments path

||| 将 URL 路径字符串分割为路径段列表
||| 移除空路径段，便于路径匹配
||| @ path 原始URL路径字符串
public export
segments : String -> List String
segments path = filter (/= "") . forget . split (== '/') $ path

||| 查找匹配请求的路由
||| 遍历服务器中的所有路由，找到第一个匹配的路由
||| 返回匹配的路由和提取的路径参数
||| @ server 服务器实例，包含路由列表
||| @ req HTTP请求对象
public export
findMatchingRoute : Server -> Request -> Maybe (Route, Params)
findMatchingRoute (MkServer routes) req = findMatchingRoute' routes
  where
    -- 内部辅助函数，递归遍历路由列表
    findMatchingRoute' : List Route -> Maybe (Route, Params)
    findMatchingRoute' [] = Nothing
    findMatchingRoute' (route :: routes) = 
      case matchAPI (segments req.uri) route.api of
        Just params => Just (route, params)
        Nothing => findMatchingRoute' routes

||| 执行处理器函数
||| 根据 API 类型和提取的参数执行对应的处理器函数
||| 将处理结果包装为响应对象
||| @ api API定义
||| @ handler 处理器函数
||| @ params 从URL中提取的参数
public export
executeHandler : (api : API) -> (handler : HandlerType api) -> 
                   (params : Params) -> IO Response
executeHandler (path :-> Get _) handler params = JSONResponse <$> handler params


||| 处理 HTTP 请求
||| 查找匹配的路由，执行处理器函数，生成响应
||| 如果没有匹配的路由，返回 404 响应
||| @ server 服务器实例
||| @ req HTTP请求对象
public export
processRequest : Server -> Request -> IO Response
processRequest server req = 
  case findMatchingRoute server req of
    Just (route, params) => executeHandler route.api route.handler params
    Nothing => pure notFoundResponse
