||| Define request types and functions
module Ope.WAI.Request

import Ope.WAI.Core
import Data.SortedMap

import Derive.Prelude

%language ElabReflection

||| HTTP method enum
||| Defines supported HTTP request methods
public export
data Method = GET | POST | HEAD

%runElab derive "Method" [Show,Eq,Ord]


||| Parse HTTP method string
||| 
||| Converts a string to a Method enum value
||| @ s Method string (e.g. "GET", "POST", etc.)
public export
method : String -> Either HTTPErr Method
method "GET"  = Right GET
method "POST" = Right POST
method "HEAD" = Right HEAD
method _      = Left InvalidRequest

||| RequestBody is the type of request body
||| It is a stream of bytes
public export
0 RequestBody : Type
RequestBody = HTTPStream ByteString

||| HTTP request record type
||| Contains all relevant information for an HTTP request
public export
record Request where
  constructor MkRequest
  ||| HTTP request method (GET, POST, etc.)
  method  : Method
  ||| Request URI path
  uri     : String
  ||| Query parameter map
  queryParams : SortedMap String String
  ||| HTTP version
  version : Version
  ||| HTTP header fields map
  headers : Headers
  ||| Request body length
  length  : Nat
  ||| Request body content type
  type    : Maybe String
  ||| Request body byte stream
  body    : RequestBody

