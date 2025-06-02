||| HTTP server implementation
module Pact.Server.HTTP

import Pact.Server.Core
import Pact.Server.Util

import public Data.SortedMap
import Data.ByteVect as BV
import public FS.Posix
import public FS.Socket
import Debug.Trace

import public IO.Async.Loop.Posix
import public IO.Async.Loop.Epoll
import public System

import Derive.Prelude

import Pact.WAI
import Pact.API

%default total

||| Prog type is the core type for the server's asynchronous stream program
||| Based on Poll event loop and AsyncStream for asynchronous IO
public export
0 Prog : List Type -> Type -> Type
Prog = AsyncStream Poll

||| Run the HTTP server
||| 
||| Accepts an asynchronous program and executes it in the epoll event loop
||| @ prog The server program to execute
public export
covering
runServer' : Prog [Errno] Void -> IO ()
runServer' prog = epollApp $ mpull (handle [stderrLn . interpolate] prog)

||| MaxHeaderSize is the maximum size of an HTTP header
||| MaxContentSize is the maximum size of an HTTP content
%inline
MaxHeaderSize, MaxContentSize : Nat
MaxHeaderSize = 0xffff
MaxContentSize = 0xffff_ffff

||| SPACE is the space character
||| COLON is the colon character
%inline
SPACE, COLON : Bits8
SPACE = 32
COLON = 58

||| Parse the start line of an HTTP request
||| 
||| Extracts HTTP method, target path, and HTTP version
||| @ bs ByteString containing the start line
startLine : ByteString -> Either HTTPErr (Method,String,Version)
startLine bs =
  case toString <$> split SPACE (trim bs) of
    [m,t,v] => [| (\x,y,z => (x,y,z)) (method m) (pure t) (version v) |]
    _       => Left InvalidRequest


||| Parse HTTP request headers
||| 
||| Recursively processes the header list and builds the header map
||| @ hs Currently parsed header map
||| @ t List of ByteStrings to parse
headers : Headers -> List ByteString -> Either HTTPErr Headers
headers hs []     = Right hs
headers hs (h::t) =
  case break (COLON ==) h of
    (xs,BS (S k) bv) =>
     let name := toLower (toString xs)
         val  := toString (trim $ tail bv)
      in headers (insert name val hs) t
    _                => Left InvalidRequest


||| Get content length from HTTP headers
||| @ hs HTTP header map
contentLength : Headers -> Nat
contentLength = maybe 0 cast . lookup "content-length"

||| Get content type from HTTP headers
||| @ hs HTTP header map
contentType : Headers -> Maybe String
contentType = lookup "content-type"


||| Assemble HTTP request object
||| 
||| Assembles parsed HTTP components into a complete Request object
||| @ p Asynchronous pull stream of HTTP ByteStrings
export
assemble :
     HTTPPull (List ByteString) (HTTPStream ByteString)
  -> HTTPPull o (Maybe Request)
assemble p = Prelude.do
  Right (h,rem) <- C.uncons p | _ => pure Nothing
  (met,uri,vrs) <- injectEither (startLine h)
  (hs,body)     <- foldPairE headers empty rem
  let cl := contentLength hs
      ct := contentType hs
      queryParams := SortedMap.fromList $ []
  when (cl > MaxContentSize) (throw ContentSizeExceeded)
  pure $ Just (MkRequest met uri queryParams vrs hs cl ct $ C.take cl body)


||| Parse HTTP request from HTTP byte stream
||| 
||| @ req HTTP byte stream
export
request : RequestBody -> HTTPPull o (Maybe Request)
request req =
     breakAtSubstring pure "\r\n\r\n" req
  |> C.limit HeaderSizeExceeded MaxHeaderSize
  |> lines
  |> assemble

||| encodeResponse' is a function that encodes an HTTP response
||| 
||| Generates an HTTP response ByteString with status code and body
||| @ status HTTP status code
||| @ body Response body content
export
encodeResponse' : Response -> ByteString
encodeResponse' res =  let bs = fromString . renderResponse $ traceVal res in bs


||| Generate 400 Bad Request response
||| @ return ByteString
export
badRequestHTTP : String -> ByteString
badRequestHTTP body = fromString $ renderResponse (badRequestResponse body)

||| serve is a function that handles a single client connection
||| 
||| Reads client request and returns response, then closes the connection
||| @ app Application handler function
||| @ cli Client socket
covering
serve : HTTPApplication -> Socket AF_INET -> Async Poll [] ()
serve app cli =
  flip guarantee (close' cli) $
    mpull $ handleErrors (\(Here x) => trace "handleRequest' error" (stderrLn "\{x}")) $
         bytes cli 0xfff
      |> request
      |> handleRequest'
  where
    response : Maybe Request -> HTTPStream ByteString
    response Nothing  = pure ()
    response (Just r) = let s1 =  app r in trace "render response" mapOutput encodeResponse' (trace "get response" s1)

    handleRequest' : HTTPPull ByteString (Maybe Request) -> AsyncStream Poll [Errno] Void
    handleRequest' p =
      extractErr HTTPErr (writeTo cli (p >>= response)) >>= \case
        Left err => trace "handleRequest' badRequestHTTP" (emit (badRequestHTTP (show err)) |> writeTo cli)
        Right () => trace "handleRequest' end" pure ()

    -- handleRequest'' : HTTPPull ByteString (Maybe Request) -> HTTPPull ByteString (Maybe Response)
    handleRequest'' p = onError (writeTo cli (p >>= response)) $ \errs => case errs of
      _ => trace "handleRequest'' end" pure ()
    
||| serverFunc is a function that creates and starts an HTTP server
||| 
||| Creates HTTP server according to the provided config and application
||| @ config Server configuration
||| @ app Application handler function
||| @ return Prog [Errno] Void
covering
public export
serverFunc : ServerConfig -> HTTPApplication -> Prog [Errno] Void
serverFunc config app =
  let conn = acceptOn AF_INET SOCK_STREAM config.bind
      serve' = serve app
  in
  case config.maxConns of
    S k => foreachPar (S k) serve' conn
    Z   => foreachPar 1 serve' conn

||| runServer is a function that runs the HTTP server
||| @ config Server configuration
||| @ server Server instance
||| @ return IO ()
covering
public export
runRouter: ServerConfig -> Router IO -> IO ()
runRouter config router = do
  let app = processRequest router
  runServer' $ serverFunc config app
