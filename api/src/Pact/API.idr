||| Define the API.
module Pact.API

import public Pact.API.Operator
import public Pact.API.Verb
import public Pact.API.MimeRender
import public Pact.API.Accept
import public Pact.API.HttpApiData
import public Pact.API.Component

import Data.Vect
import public Data.Vect.Quantifiers

public export
record API where
  constructor (:>)
  { types : Vect typesLen Type }
  { auto prf : All FromHttpApiData types }
  paths : Component lastType types reqBody
  verb : Verb

public export
ApiReqBody : API -> Type
ApiReqBody (path :> _) = PathReqBody path


||| Get the type of the handler.
||| @m The type of the monad.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the handler.
public export
GetHandlerType : (m : Type -> Type) -> API -> Type
GetHandlerType m (path :> ep) = GetPathType path $ (m (VerbResponse ep))

||| Get the type of the endpoint.
||| @m The type of the monad.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the endpoint.
public export
GetEPFromAPI : (m : Type -> Type) -> API -> Type
GetEPFromAPI m (path :> ep) = m (VerbResponse ep)

||| Get the type of the endpoint result.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the endpoint result.
public export
GetEpResultTypeFromAPI : API -> Type
GetEpResultTypeFromAPI (path :> ep) = VerbResponse ep