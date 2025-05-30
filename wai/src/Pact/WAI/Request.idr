||| Define request types and functions
module Pact.WAI.Request

import Pact.WAI.Core
import public Data.SortedMap
import Pact.WAI.Method
import Pact.WAI.Version
import Pact.WAI.Header

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
  queryParams : QueryParams
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

