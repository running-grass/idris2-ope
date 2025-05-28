module Pact.API.Endpoint

import Pact.WAI.Request

||| Endpoint represents an API endpoint, including HTTP method and response type
||| The resp parameter type carries response type information
public export
data Endpoint : Type -> Type -> Type where
  -- No Request body and Response body

  ||| CONNECT request endpoint, no request body and response body
  CONNECT: Endpoint () ()

  ||| HEAD request endpoint, no request body and response body
  HEAD: Endpoint () ()


  -- No Request body, but has Response body

  ||| OPTIONS request endpoint, no request body and response body
  OPTIONS: (resp : Type) -> Endpoint () resp

  ||| TRACE request endpoint, no request body and response body
  TRACE: (resp : Type) -> Endpoint () resp
  
  ||| GET request endpoint, resp specifies the return type
  ||| For example: Get String, Get User, Get (List Product)
  Get : (resp : Type) -> Endpoint () resp
  
   -- Has Request body and Response body

  ||| POST request endpoint, req specifies the request type, resp specifies the return type
  ||| For example: Post User String, Post User (List Product)
  Post : (req : Type) -> (resp : Type) -> Endpoint req resp

  ||| PUT request endpoint, req specifies the request type, resp specifies the return type
  ||| For example: Put User String, Put User (List Product)
  Put : (req : Type) -> (resp : Type) -> Endpoint req resp

  ||| DELETE request endpoint, req specifies the request type, resp specifies the return type
  ||| For example: Delete User String, Delete User (List Product)
  Delete : (req : Type) -> (resp : Type) -> Endpoint req resp

  ||| PATCH request endpoint, req specifies the request type, resp specifies the return type
  ||| For example: Patch User String, Patch User (List Product)
  Patch : (req : Type) -> (resp : Type) -> Endpoint req resp

||| Convert an Endpoint to a Method
public export
endpointToMethod : Endpoint req resp -> Method
endpointToMethod HEAD = HEAD
endpointToMethod CONNECT = CONNECT
endpointToMethod (OPTIONS resType) = OPTIONS
endpointToMethod (TRACE resType) = TRACE
endpointToMethod (Get resType) = GET
endpointToMethod (Post reqType resType) = POST
endpointToMethod (Put reqType resType) = PUT
endpointToMethod (Delete reqType resType) = DELETE
endpointToMethod (Patch reqType resType) = PATCH


||| EndpointResult is a type function that computes the response type of an endpoint
||| It extracts the response type information from the Endpoint type
public export
EndpointResult : {resp : Type} -> Endpoint req resp -> Type
EndpointResult _ = resp

public export
GetEndpointType : Endpoint req resp -> Type
GetEndpointType HEAD = IO ()
GetEndpointType CONNECT = IO ()

GetEndpointType (OPTIONS resType) = IO resType
GetEndpointType (TRACE resType) = IO resType
GetEndpointType (Get resType) = IO resType

GetEndpointType (Post reqType resType) = IO resType
GetEndpointType (Put reqType resType) = IO resType
GetEndpointType (Delete reqType resType) = IO resType
GetEndpointType (Patch reqType resType) = IO resType