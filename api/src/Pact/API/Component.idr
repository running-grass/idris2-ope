||| Define the component of a path.
module Pact.API.Component

import Pact.API.Operator
import Pact.API.HttpApiData
import Pact.API.Verb

import Data.Vect
import Data.Vect.Quantifiers

public export
data Component : Type -> Vect n Type -> (reqBody : Type) -> Type where
  ||| Static path segment, e.g. "users"
  StaticPath : String -> Component () [()] Void
  ||| Captured path segment, e.g. ":id"
  Capture : String -> (a : Type) -> Component a [a] Void
  ||| Request body, e.g. UserCreateRequest
  ReqBody : (a : Type) -> Component () [()] a
  ||| Concatenate two paths, e.g. "users/:id/todos"
  (:/) : Component tl [tl] Void -> Component tr tsr reqBody -> Component tl (tl :: tsr) reqBody

public export
isReqBody : Component t ts reqBody -> Bool
isReqBody (ReqBody _) = True
isReqBody _ = False

public export 
PathReqBody : Component t ts reqBody -> Type
PathReqBody (StaticPath _) = Void
PathReqBody (Capture _ _) =  Void
PathReqBody (ReqBody reqBody) =  reqBody
-- PathReqBody (ReqBody reqBody :/ restPath) = reqBody
PathReqBody (path :/ restPath) = PathReqBody restPath

public export
hasReqBody : Component t ts reqBody -> Bool
hasReqBody (StaticPath _) = False
hasReqBody (Capture _ _) = False
hasReqBody (ReqBody _) = True
hasReqBody (_ :/ restPath) = hasReqBody restPath


||| Get the name of a capture path segment
||| @ path The path to get the name of
||| @ return The name of the capture path segment
public export
getCaptureName : Component t _ _ -> String
getCaptureName (Capture s _) = s
getCaptureName _ = "unknown"

||| Match a path against a list of path segments.
|||
||| @path The path to match.
||| @segs The list of path segments.
||| @allprf A proof that all the path segments are path parameters.
|||
||| Returns a list of the parsed path parameters.
public export
matchPath : Component t ts reqBody' -> Vect m String -> { auto allprf : All FromHttpApiData ts } -> Either String (HVect ts)
matchPath (StaticPath s) [seg] = if s == seg then Right [()] else Left "Path mismatch"
matchPath (ReqBody s) [] = Right [()]
matchPath (Capture s t) [seg] { allprf = prf :: restPrf } = case parseUrlPiece seg  of
  Right val => Right [val]
  Left err => Left ("Failed to parse path parameter:" ++ s ++ " " ++ err)
matchPath (StaticPath s :/ restPath) (seg :: segs) { allprf = prf :: restPrf } = if s == seg then map (\xs => () :: xs) (matchPath restPath segs) else Left "Path mismatch"
matchPath (Capture s t :/ restPath) (seg :: segs) { allprf = prf :: restPrf } = case parseUrlPiece seg of
  Right val => case matchPath restPath segs of
    Right vals => Right (val :: vals)
    Left err => Left err
  Left err => Left ("Failed to parse path parameter:" ++ s ++ " " ++ err)
matchPath _ segs = Left ("unknown path" ++ show segs)

||| Get the type of the path.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the path.
public export
GetPathType : Component _ ts reqBody' -> (epType : Type) -> Type
GetPathType (StaticPath _) epType = epType
GetPathType (Capture _ t) epType = t -> epType
GetPathType (ReqBody reqBody) epType = reqBody -> epType
GetPathType { ts = t :: ts'} (path :/ restPath) epType = let epType' = GetPathType restPath epType in case path of
  StaticPath _ => epType'
  Capture name t => t -> epType'
  -- ReqBody reqBody => reqBody -> epType'
  _ => epType'