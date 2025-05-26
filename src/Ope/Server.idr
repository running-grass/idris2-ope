||| The Server module provides the core functionality of the HTTP server
||| Implements an asynchronous IO-based HTTP server framework
module Ope.Server

import public Data.SortedMap
import Data.ByteVect as BV

import public FS.Posix
import public FS.Socket

import public IO.Async.Loop.Posix
import public IO.Async.Loop.Epoll

import public System

import Derive.Prelude

import Ope.Server.Type

import Ope.WAI.Core
import Ope.WAI.Request
import Ope.WAI.Response

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
export covering
runServer : Prog [Errno] Void -> IO ()
runServer prog = epollApp $ mpull (handle [stderrLn . interpolate] prog)


%inline
MaxHeaderSize, MaxContentSize : Nat
MaxHeaderSize = 0xffff
MaxContentSize = 0xffff_ffff

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
request : HTTPStream ByteString -> HTTPPull o (Maybe Request)
request req =
     breakAtSubstring pure "\r\n\r\n" req
  |> C.limit HeaderSizeExceeded MaxHeaderSize
  |> lines
  |> assemble


getContentType : Response -> String
getContentType (JSONResponse _) = "application/json"
getContentType (PlainTextResponse _) = "text/plain"

||| Encode HTTP response
||| 
||| Generates an HTTP response ByteString with status code and body
||| @ status HTTP status code
||| @ body Response body content
export
encodeResponse' : (status : Nat) -> Response -> ByteString
encodeResponse' status response =
  fromString $
    statusStr ++
    contentLengthStr ++
    contentTypeStr ++
    "\r\n" ++
    body
  where
    body : String
    body = renderResponse response

    contentTypeStr : String
    contentTypeStr = "Content-Type: \{getContentType response}\r\n"
    statusStr : String
    statusStr = "HTTP/1.1 \{show status}\r\n"
    contentLengthStr : String
    contentLengthStr = "Content-Length: \{show (length body)}\r\n"


||| Generate 400 Bad Request response
export
badRequest : ByteString
badRequest = encodeResponse' 400 (PlainTextResponse "Bad Request")

||| Handle a single client connection
||| 
||| Reads client request and returns response, then closes the connection
||| @ app Application handler function
||| @ cli Client socket
covering
serve : (Request -> HTTPStream Response) -> Socket AF_INET -> Async Poll [] ()
serve app cli =
  flip guarantee (close' cli) $
    mpull $ handleErrors (\(Here x) => stderrLn "\{x}") $
         bytes cli 0xfff
      |> request
      |> handleRequest'
  where
    response : Maybe Request -> HTTPStream ByteString
    response Nothing  = pure ()
    response (Just r) =  app r |> mapOutput (encodeResponse' 200)

    handleRequest' : HTTPPull ByteString (Maybe Request) -> AsyncStream Poll [Errno] Void
    handleRequest' p =
      extractErr HTTPErr (writeTo cli (p >>= response)) >>= \case
        Left _   => emit badRequest |> writeTo cli
        Right () => pure ()


||| Server configuration record type
||| 
||| Contains server bind address and max connection count
public export
record ServerConfig where
  constructor MkServerConfig
  ||| Server bind IP address and port
  bind : IP4Addr
  ||| Max allowed connections
  maxConns : Nat

||| Default server configuration
||| 
||| Binds to 127.0.0.1:2222, max connections 128
public export
defaultConfig : ServerConfig
defaultConfig = MkServerConfig  {
    bind = IP4 [127,0,0,1] 2222,
    maxConns = 128
  }

||| Create and start HTTP server
||| 
||| Creates HTTP server according to the provided config and application
||| @ config Server configuration
||| @ app Application handler function
covering
public export
server : ServerConfig -> (Request -> HTTPStream Response) -> Prog [Errno] Void
server config app =
  let conn = acceptOn AF_INET SOCK_STREAM config.bind
      serve' = serve app
  in
  case config.maxConns of
    S k => foreachPar (S k) serve' conn
    Z   => foreachPar 1 serve' conn
