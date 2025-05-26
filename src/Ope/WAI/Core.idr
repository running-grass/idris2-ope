||| Define core types and functions
module Ope.WAI.Core

import Derive.Prelude
import Data.SortedMap

import FS.Posix
import FS.Socket

import IO.Async.Loop.Posix
import IO.Async.Loop.Epoll
import public Data.ByteVect

%language ElabReflection

||| HTTP error type
||| Represents possible errors during HTTP processing
public export
data HTTPErr : Type where
  ||| Header size exceeded
  HeaderSizeExceeded  : HTTPErr
  ||| Content size exceeded
  ContentSizeExceeded : HTTPErr
  ||| Invalid HTTP request format
  InvalidRequest      : HTTPErr

%runElab derive "HTTPErr" [Show,Eq,Ord]

||| String interpolation implementation for HTTPErr
export
Interpolation HTTPErr where
  interpolate HeaderSizeExceeded  = "header size exceeded"
  interpolate ContentSizeExceeded = "content size exceeded"
  interpolate InvalidRequest      = "invalid HTTP request"

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
||| HTTP version enum
||| Defines supported HTTP protocol versions
public export
data Version = V10 | V11 | V20

%default total
%default covering
%runElab derive "Version" [Show,Eq,Ord]
||| Parse HTTP version string
||| 
||| Convert string to Version enum value
||| @ s Version string (e.g. "HTTP/1.0", "HTTP/1.1", etc.)
public export
version : String -> Either HTTPErr Version
version "HTTP/1.0" = Right V10
version "HTTP/1.1" = Right V11
version "HTTP/2.0" = Right V20
version _          = Left InvalidRequest

||| Content type enum
||| Similar to Servant's JSON/PlainText content types
public export
data ContentType = JSON | PlainText
