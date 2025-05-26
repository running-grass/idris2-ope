||| Define types and functions for Web Application Interface
module Ope.WAI

import public Ope.WAI.Core
import public Ope.WAI.Request
import public Ope.WAI.Response

import FS.Posix
import IO.Async.Loop.Posix

||| HTTPResponse is the type of HTTP response.
public export
0 HTTPResponse : Type
HTTPResponse = HTTPStream Response

||| HTTPApplication is the type of HTTP application.
public export
0 HTTPApplication : Type
HTTPApplication = Request -> HTTPResponse

||| Default empty response, usually used for testing or as a placeholder for unimplemented endpoints.
public export
emptyResponse : HTTPResponse
emptyResponse = emit $ PlainTextResponse "OK"