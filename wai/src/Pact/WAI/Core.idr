||| Define core types and functions
module Pact.WAI.Core

import Derive.Prelude
import public Data.SortedMap

import FS.Posix
import FS.Socket

import IO.Async.Loop.Epoll

import public Data.ByteVect
import public System.Posix.Errno.Type
import public IO.Async.Loop.Posix
import Pact.WAI.HTTPErr

%default total
%default covering

%language ElabReflection

||| HTTP pull stream type
||| Represents an asynchronous stream from which HTTP data can be pulled
||| @ o Output type
||| @ r Result type
public export
0 HTTPPull : Type -> Type -> Type
HTTPPull o r = AsyncPull Poll o [Errno,HTTPErr] r

||| HTTP byte stream type
||| Represents an HTTP stream from which bytes can be pulled
||| @ o Output type
public export
0 HTTPStream : Type -> Type
HTTPStream o = AsyncPull Poll o [Errno,HTTPErr] ()


||| HTTP header type
||| Represented as a key-value map, both key and value are strings
public export
0 Headers : Type
Headers = SortedMap String String

||| HTTP Query parameter type
||| Represented as a key-value map, both key and value are strings
public export
0 QueryParams : Type
QueryParams = SortedMap String String
||| Content type enum
||| Similar to Servant's JSON/PlainTextAccept content types
public export
data ContentType = JSON | PlainTextAccept

||| Parse content type string
||| @ s Content type string (e.g. "application/json", "text/plain", etc.)
public export
parseContentType : String -> Either HTTPErr ContentType
parseContentType "application/json" = Right JSON
parseContentType "text/plain"      = Right PlainTextAccept
parseContentType _                 = Left InvalidRequest
