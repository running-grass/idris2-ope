module Pact.API.Endpoint


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

||| EndpointResult is a type function that computes the response type of an endpoint
||| It extracts the response type information from the Endpoint type
public export
EndpointResult : {resp : Type} -> Endpoint req resp -> Type
EndpointResult _ = resp


public export
GetEpResultType: Endpoint req resp -> Type
GetEpResultType CONNECT = ()
GetEpResultType HEAD = ()
GetEpResultType (OPTIONS resType) = resType
GetEpResultType (TRACE resType) = resType
GetEpResultType (Get resType) = resType
GetEpResultType (Post reqType resType) = resType
GetEpResultType (Put reqType resType) = resType
GetEpResultType (Delete reqType resType) = resType
GetEpResultType (Patch reqType resType) = resType

public export
GetEndpointType : (m : Type -> Type) -> Endpoint req resp -> Type
GetEndpointType m ep = m (GetEpResultType ep)