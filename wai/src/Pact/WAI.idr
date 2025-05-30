||| Define types and functions for Web Application Interface
module Pact.WAI

import public Pact.WAI.Core
import public Pact.WAI.Request
import public Pact.WAI.Response
import public Pact.WAI.Method
import public Pact.WAI.HTTPErr
import public Pact.WAI.Version

import public FS.Core
import public FS.Concurrent

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

public export
noContentResponse : HTTPResponse
noContentResponse = emit NoContentResponse