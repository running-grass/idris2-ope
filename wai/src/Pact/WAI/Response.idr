||| Define response types and functions
module Pact.WAI.Response

import Pact.WAI.Core

import public JSON.FromJSON
import public JSON.ToJSON

||| Response data type
||| Unified handling of different response types
public export
data Response : Type where
  ||| JSON response, directly uses a string to represent JSON
  JSONResponse : ToJSON a => a -> Response
  ||| Plain text response, requires value type to implement Show interface
  PlainTextResponse : Show a => a -> Response

||| 404 Not Found response
||| Returned when the requested path cannot be matched to any route
public export
notFoundResponse : Response
notFoundResponse = PlainTextResponse "Not Found"

||| 400 Bad Request response
||| Returned when the request is invalid
public export
badRequestResponse : Response
badRequestResponse = PlainTextResponse "Bad Request"

||| Response rendering function
||| Converts a Response to a string for HTTP response
||| @ resp The response object to render
public export
renderResponse : Response -> String
renderResponse (JSONResponse a) = encode a
renderResponse (PlainTextResponse value) = show value