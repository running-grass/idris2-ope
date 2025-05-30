||| Define response types and functions
module Pact.WAI.Response

import Pact.WAI.Core
import Pact.WAI.Header
import Pact.WAI.StatusCode
import Data.String
import Data.SortedMap as Map

public export
record Response where
  constructor MkResponse
  status : StatusCode
  headers : Headers
  body : Maybe String

||| 404 Not Found response
||| Returned when the requested path cannot be matched to any route
public export
notFoundResponse : Response
notFoundResponse = MkResponse notFound emptyHeaders Nothing

||| 400 Bad Request response
||| Returned when the request is invalid
public export
badRequestResponse : Response
badRequestResponse = MkResponse badRequest emptyHeaders Nothing

||| Render headers to a string
||| @ headers The headers to render
public export
renderHeaders : Headers -> String
renderHeaders headers = 
  let headersList = Map.toList headers
  in Data.String.joinBy "\r\n" . map (\(k, v) => "\{k}: \{v}" ) $ headersList

||| Response rendering function
||| Converts a Response to a string for HTTP response
||| @ resp The response object to render
public export
renderResponse : Response -> String
renderResponse (MkResponse status headers body) = 
  let statusLine = "HTTP/1.1 \{status}\r\n"
      bodyStr = maybe "" id body
  in statusLine ++ renderHeaders headers ++ "\r\n\r\n" ++ bodyStr
  

testResponse : Response
testResponse = MkResponse ok (fromList [("Content-Type", "text/plain"), ("Content-Length", "12")]) (Just "Hello, World!")

test = renderResponse testResponse