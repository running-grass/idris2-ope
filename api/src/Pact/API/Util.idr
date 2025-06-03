module Pact.API.Util

import Data.Vect
import Data.Vect.Quantifiers

import Pact.API.Operator
import Pact.API.HasPathParam
import Pact.API.Core
import Pact.API.Verb

||| Match a path against a list of path segments.
|||
||| @path The path to match.
||| @segs The list of path segments.
||| @allprf A proof that all the path segments are path parameters.
|||
||| Returns a list of the parsed path parameters.
public export
matchPath : Path t ts reqBody' -> Vect m String -> { auto allprf : All HasPathParam ts } -> Either String (HVect ts)
matchPath (StaticPath s) [_] { allprf = prf :: restPrf } = Right [()]
matchPath (ReqBody s) [] = Right [()]
matchPath (Capture s t) [seg] { allprf = prf :: restPrf } = case parsePathParams seg  of
  Just val => Right [val]
  Nothing => Left ("Failed to parse path parameter:" ++ s)
-- matchPath (ReqBody reqBody :/ restPath) segs { allprf = _ :: restPrf } = case matchPath restPath segs { allprf = restPrf } of
--   Right vals => Right (() :: vals)
--   Left err => Left err
matchPath (path :/ restPath) (seg :: segs) { allprf = prf :: restPrf } = case mVal of
  Just val => case matchPath restPath segs of
    Right vals => Right (val :: vals)
    Left err => Left err
  Nothing => Left ("Failed to parse path parameter:" ++ getCaptureName path)

-- case (path, mVal) of
--  (_, (Just val)) => case matchPath restPath segs of
--     Right vals => Right (val :: vals)
--     Left err => Left err
--   (_, Nothing) => Left ("Failed to parse path parameter:" ++ getCaptureName path)
  where
  mVal : Maybe t
  mVal = parsePathParams seg
matchPath _ segs = Left ("unknown path" ++ show segs)

||| Get the type of the path.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the path.
public export
GetPathType : Path _ ts reqBody' -> (epType : Type) -> Type
GetPathType (StaticPath _) epType = epType
GetPathType (Capture _ t) epType = t -> epType
GetPathType (ReqBody reqBody) epType = reqBody -> epType
GetPathType { ts = t :: ts'} (path :/ restPath) epType = let epType' = GetPathType restPath epType in case path of
  StaticPath _ => epType'
  Capture name t => t -> epType'
  -- ReqBody reqBody => reqBody -> epType'
  _ => epType'

||| Get the type of the handler.
||| @m The type of the monad.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the handler.
public export
GetHandlerType : (m : Type -> Type) -> API ts -> Type
GetHandlerType m (path :> ep) = GetPathType path $ (m (VerbResponse ep))

||| Get the type of the endpoint.
||| @m The type of the monad.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the endpoint.
public export
GetEPFromAPI : (m : Type -> Type) -> API tss -> Type
GetEPFromAPI m (path :> ep) = m (VerbResponse ep)

||| Get the type of the endpoint result.
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the endpoint result.
public export
GetEpResultTypeFromAPI : API ts -> Type
GetEpResultTypeFromAPI (path :> ep) = VerbResponse ep