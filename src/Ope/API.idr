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
import JSON.FromJSON

fillDefault: Lazy a -> Maybe a -> Maybe a
fillDefault def Nothing = Just def
fillDefault _ (Just a) = Just a

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
    method : Method
    method = req.method

    matchPath : Maybe Params -> List String -> Path -> Maybe Params
    matchPath Nothing [] _ = Nothing
    matchPath prevParams [s] (StaticPath path :> Nil) = 
        if s == path then (fillDefault emptyParams prevParams) else Nothing
    matchPath prevParams [s] (Capture key ct :> Nil) = insert key s <$> fillDefault emptyParams prevParams
    matchPath prevParams (s :: segments) (path :> rest) = 
      case matchPath prevParams [s] (path :> Nil) of
        Just params => matchPath (Just params) segments rest
        Nothing => Nothing
    matchPath _ _ _ = Nothing

    -- 合并后的路径匹配和参数提取逻辑
    matchAPIPath : List String -> API -> Maybe Params
    matchAPIPath segments (path :-> (Get _)) = case method of
      GET => matchPath Nothing segments path
      _ => Nothing
    matchAPIPath segments (path :-> (Post reqType resType)) = case method of
      POST => matchPath Nothing segments path
      _ => Nothing

    -- 内部辅助函数，递归遍历路由列表
    findMatchingRoute' : List Route -> Maybe (Route, Params)
    findMatchingRoute' [] = Nothing
    findMatchingRoute' (route :: routes) = 
      case matchAPIPath (segments req.uri) route.api of
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
executeHandler (path :-> (Post reqType resType)) handler params = 

  case req of
    Left err => pure badRequestResponse
    Right req' => JSONResponse <$> handler params req'
  where
    req : Either DecodingErr reqType
    req = decode $ encode params


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
