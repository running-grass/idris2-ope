||| Define Route and Handler types
module Pact.API.Core
import public Pact.API.Operator
import Data.SortedMap
import JSON.Derive
import Pact.WAI.Request
import public Pact.API.Endpoint
import public Pact.API.HasPathParam

%language ElabReflection

%default total


||| PathSegment represents a segment of an API path
||| It can be a static path segment or a captured path segment
public export
data PathSegment : Type where
  ||| Static path segment, e.g. "users"
  StaticPath : String -> PathSegment
  ||| Captured path segment, e.g. ":id"
  Capture : String -> (a : Type) -> PathSegment

||| PathSegment implements the FromString interface
||| It allows converting a string to a PathSegment
public export
implementation FromString (PathSegment) where
  fromString = StaticPath

||| Params is a type for query parameters
||| It is a map of parameter names to their values
public export
Params : Type
Params = SortedMap String String

||| Query represents a type-safe description of an API path
||| By combining Query constructors, you can build a complete route path
public export
data Query : Type where
  ||| Empty query params, e.g. ""
  Nil : Query
  ||| Query params record, e.g. "?all"
  -- QueryAll : Params -> Query [] Params
  ||| Query params composition operator, used to connect two paths
  ||| For example: "api" :/ "users" :/ QueryAll User
  (:/) : PathSegment -> Query -> Query -- Query composition


||| API type combines path and endpoint into a complete API description
||| It is the core of the type-safe API framework
public export
data API : Type where
  ||| Connects path and endpoint to form a complete API
  ||| Show resp constraint ensures the response type can be serialized
  ||| For example: StaticPath "users" :/ Get (List User)
  (:->) : (FromJSON req, ToJSON resp, Show resp) 
       => (Query) -> Endpoint req resp -> API

||| emptyParams is a function that returns an empty Params map
||| It is used as a default value for query parameters
public export
emptyParams : Params
emptyParams = empty

||| HandlerType computes the handler function type required for a specific API
||| It is used to determine the type of the handler function for a given API
||| For simple APIs, the handler type is just the return type
||| For APIs with parameters, the handler type will include parameter types
||| For example: HandlerType for API "users/:id" will be (id -> User)
public export
HandlerType : API -> Type
HandlerType (path :-> HEAD) = Params -> IO ()
HandlerType (path :-> CONNECT) = Params -> IO ()

HandlerType (path :-> (OPTIONS resType)) = Params -> IO resType
HandlerType (path :-> (TRACE resType)) = Params -> IO resType
HandlerType (path :-> (Get resType)) = Params -> IO resType

HandlerType (path :-> (Post reqType resType)) = Params -> reqType -> IO resType
HandlerType (path :-> (Put reqType resType)) = Params -> reqType -> IO resType
HandlerType (path :-> (Delete reqType resType)) = Params -> reqType -> IO resType
HandlerType (path :-> (Patch reqType resType)) = Params -> reqType -> IO resType


||| Route record type
||| Associates an API definition with its handler function
public export
record Route where
  constructor (:=>)
  ||| API definition, describes path and endpoint
  api: API
  ||| Handler function, type is determined by the API definition
  handler : HandlerType api

||| Server data type
||| Contains a set of route definitions for handling HTTP requests
public export
data Server : Type where
  ||| Creates a server instance containing a list of routes
  MkServer : (routes : List Route) -> Server