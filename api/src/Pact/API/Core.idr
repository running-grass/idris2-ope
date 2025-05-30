module Pact.API.Core

import Pact.API.Operator
import Data.Vect
import Pact.API.HasPathParam
import Pact.API.Verb

import Data.Vect.Quantifiers

public export
data Path : Type -> Vect n Type -> Type where
  ||| Static path segment, e.g. "users"
  StaticPath : String -> Path () [()]
  ||| Captured path segment, e.g. ":id"
  Capture : String -> (a : Type) -> Path a [a]

  (:/) : Path tl [tl] -> Path tr tsr -> Path tl (tl :: tsr)


public export
record API (types: Vect n Type) where
  constructor (:>)
  path : Path lastType types
  verb : Verb
  { auto prf : All HasPathParam types }

||| Get the name of a capture path segment
||| @ path The path to get the name of
||| @ return The name of the capture path segment
public export
getCaptureName : Path t ts -> String
getCaptureName (Capture s _) = s
getCaptureName _ = "unknown"