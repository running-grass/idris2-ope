||| HTTP methods
module Pact.WAI.Method

import Derive.Prelude
import Pact.WAI.HTTPErr

%language ElabReflection

||| HTTP method enum
||| Defines supported HTTP request methods
public export
data Method =
  HEAD | CONNECT 
  | OPTIONS | TRACE | GET
  | POST | PUT | PATCH | DELETE

%runElab derive "Method" [Show,Eq,Ord]


||| Parse HTTP method string
||| 
||| Converts a string to a Method enum value
||| @ s Method string (e.g. "GET", "POST", etc.)
public export
method : String -> Either HTTPErr Method
method "HEAD" = Right HEAD
method "CONNECT" = Right CONNECT

method "OPTIONS" = Right OPTIONS
method "TRACE" = Right TRACE
method "GET" = Right GET

method "POST" = Right POST
method "PUT" = Right PUT
method "PATCH" = Right PATCH
method "DELETE" = Right DELETE
method _ = Left InvalidRequest

