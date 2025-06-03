module Pact.API.Core

import Pact.API.Operator
import Data.Vect
import Pact.API.HasPathParam
import Pact.API.Verb

import Data.Vect.Quantifiers

public export
data Path : Type -> Vect n Type -> (reqBody : Type) -> Type where
  ||| Static path segment, e.g. "users"
  StaticPath : String -> Path () [()] Void
  ||| Captured path segment, e.g. ":id"
  Capture : String -> (a : Type) -> Path a [a] Void
  ||| Request body, e.g. "body"
  ReqBody : (a : Type) -> Path () [()] a

  (:/) : Path tl [tl] Void -> Path tr tsr reqBody -> Path tl (tl :: tsr) reqBody

public export
isReqBody : Path t ts reqBody -> Bool
isReqBody (ReqBody _) = True
isReqBody _ = False

public export 
PathReqBody : Path t ts reqBody -> Type
PathReqBody (StaticPath _) = Void
PathReqBody (Capture _ _) =  Void
PathReqBody (ReqBody reqBody) =  reqBody
-- PathReqBody (ReqBody reqBody :/ restPath) = reqBody
PathReqBody (path :/ restPath) = PathReqBody restPath

public export
hasReqBody : Path t ts reqBody -> Bool
hasReqBody (StaticPath _) = False
hasReqBody (Capture _ _) = False
hasReqBody (ReqBody _) = True
hasReqBody (_ :/ restPath) = hasReqBody restPath

public export
record API (types: Vect n Type) where
  constructor (:>)
  path : Path lastType types reqBody
  verb : Verb
  { auto prf : All HasPathParam types }

||| Get the name of a capture path segment
||| @ path The path to get the name of
||| @ return The name of the capture path segment
public export
getCaptureName : Path t _ _ -> String
getCaptureName (Capture s _) = s
getCaptureName _ = "unknown"



public export
ApiReqBody : API ts -> Type
ApiReqBody (path :> _) = PathReqBody path