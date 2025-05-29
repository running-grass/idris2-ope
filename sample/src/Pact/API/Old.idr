||| Define functions for route matching and request handling
module Pact.API.Old

import Data.String
import Data.List1
import Data.SortedMap

import public Pact.API.Operator
import public Pact.API.OldCore
import public Pact.API.Endpoint
import Pact.Server.Core
import Pact.Server.Util

import FS.Posix
import FS.Socket

import IO.Async.Loop.Posix
import IO.Async.Loop.Epoll
import FS.Concurrent

import public Pact.WAI
import JSON.ToJSON
import JSON.FromJSON


import Data.Vect

||| fillDefault is a function that fills a default value if the Maybe is Nothing
||| @ def Default value
||| @ maybe Maybe value
||| @ return Maybe value
public export
fillDefault: Lazy a -> Maybe a -> Maybe a
fillDefault def Nothing = Just def
fillDefault _ (Just a) = Just a

||| Split a URL path string into a list of segments
||| Remove empty segments for easier path matching
||| @ path Original URL path string
public export
segments : String -> List String
segments path = filter (/= "") . forget . split (== '/') $ path

matchPath : List String -> Query -> Bool
matchPath [] _ = False
matchPath [s] (StaticPath path :/ _) = 
        if s == path then True else False
matchPath [s] (Capture key ct :/ _) = True
matchPath (s :: segments) (path :/ rest) = 
    case matchPath' s path of
      True => matchPath segments rest
      False => False

      where 
        matchPath' : String -> PathSegment -> Bool
        matchPath' s (StaticPath path) = 
          if s == path then True else False
        matchPath' s (Capture key ct) = True
matchPath _ _ = False


||| Find the matching route for a request
||| Traverse all routes in the server and find the first matching one
||| Returns the matching route and extracted path parameters
||| @ server Server instance containing the route list
||| @ req HTTP request object
public export
findMatchingRoute : Server -> Request -> Maybe Route
findMatchingRoute (MkServer routes) req = findMatchingRoute' routes
  where
    method : Method
    method = req.method

    -- 合并后的路径匹配和参数提取逻辑
    matchAPIPath : List String -> API -> Bool
    matchAPIPath segments (path :-> endpoint) = case method == endpointToMethod endpoint of
      True => matchPath segments path
      False => False

    -- 内部辅助函数，递归遍历路由列表
    findMatchingRoute' : List Route -> Maybe (Route)
    findMatchingRoute' [] = Nothing
    findMatchingRoute' (route :: routes) = 
      case matchAPIPath (segments req.uri) route.api of
        True => Just route
        False => findMatchingRoute' routes


private
noResponse: (Params -> IO ()) -> Params -> HTTPResponse
noResponse handler' params = liftIO (handler' params) >>= emit . JSONResponse

private
justResponse: ToJSON a => (Params -> IO a) -> Params -> HTTPResponse
justResponse handler' params = liftIO (handler' params) >>= emit . JSONResponse


private
withRequestBody: ToJSON a => FromJSON r => (Params -> r -> IO a) -> RequestBody -> Params -> HTTPResponse
withRequestBody handler' reqBody params = bind hand reqBody
  where
    hand : ByteString -> HTTPStream Response
    hand bs = do
      let reqBody = toString bs
      let req : Either DecodingErr r = decode reqBody
      case req of
        Left err => emit badRequestResponse
        Right req' => do
          res <- liftIO $ handler' params req'
          emit $ JSONResponse res

 

||| Execute the handler function
||| Executes the corresponding handler function according to the API type and extracted parameters
||| Wraps the result as a response object
||| @ api API definition
||| @ handler Handler function
||| @ params Parameters extracted from the URL
public export
executeHandler : (api : API) -> (handler : HandlerType api) -> 
                  (req : Request) -> (params : Params)  -> HTTPResponse
executeHandler (path :-> endpoint) handler req params = do
  case endpoint of
    HEAD => noResponse handler params
    CONNECT => noResponse handler params
   
    OPTIONS resType => justResponse handler params
    TRACE resType => justResponse handler params
    Get resType => justResponse handler params
  
    Post reqType resType => withRequestBody handler req.body params
    Put reqType resType => withRequestBody handler req.body params
    Delete reqType resType => withRequestBody handler req.body params
    Patch reqType resType => withRequestBody handler req.body params


||| Handle HTTP request
||| Find the matching route, execute the handler function, and generate a response
||| If no matching route is found, return a 404 response
||| @ server Server instance
||| @ req HTTP request object
public export
processRequest : Server -> HTTPApplication
processRequest server req = 
  case findMatchingRoute server req of
    Just route => executeHandler route.api route.handler req emptyParams
    Nothing => emit notFoundResponse
