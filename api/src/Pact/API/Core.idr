module Pact.API.Core

import Pact.API.Operator
import Data.Vect
import Pact.API.HasPathParam
import Pact.API.Endpoint

import public JSON.FromJSON
import public JSON.ToJSON

import Data.Vect.Quantifiers

public export
data Path : Type -> Vect n Type -> Type where
  ||| Static path segment, e.g. "users"
  StaticPath : String -> Path () [()]
  ||| Captured path segment, e.g. ":id"
  Capture : String -> (a : Type) -> Path a [a]

  (:/) : Path tl [tl] -> Path tr tsr -> Path tl (tl :: tsr)


public export
data API : ( ts: Vect n Type) -> Type where
  (:>) : (FromJSON req, ToJSON resp) 
     => (Path t ts) -> { auto prf : All HasPathParam ts } -> Endpoint req resp -> API ts


||| Get the name of a capture path segment
||| @ path The path to get the name of
||| @ return The name of the capture path segment
public export
getCaptureName : Path t ts -> String
getCaptureName (Capture s _) = s
getCaptureName _ = "unknown"