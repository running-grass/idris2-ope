module Pact.API.Exp

import Pact.API.Core
import Data.Vect
import Data.Vect.Quantifiers
import JSON
import Pact.API.Endpoint

import JSON.Derive

%language ElabReflection
%hide Pact.API.Core.API

public export
data Path : Type -> Vect (S n) Type -> Type where
  ||| Static path segment, e.g. "users"
  StaticPath : String -> Path () [()]
  ||| Captured path segment, e.g. ":id"
  Capture : String -> (a : Type) -> Path a [a]

  (:/) : Path tl tsl -> Path tr tsr -> Path tl (tl :: tsr)

||| Parse a vector of path parameters into a vector of types.
|||
||| @pts The types of the path parameters.
||| @allprf A proof that all the path parameters are path parameters.
||| @raw The raw path parameters.
|||
||| Returns a vector of the parsed path parameters.
public export
parsePathParams : { n : Nat } -> ( pts: Vect n Type ) ->  {auto allprf :  All HasPathParam pts } -> (raw : Vect n String) -> Maybe (HVect pts)
parsePathParams [] []  = Just []
parsePathParams {n=S n} (t :: ts)  { allprf = prf :: prfs } (r :: rs) = case mVal of
  Just val => case parsePathParams ts rs of
    Just vals => Just (val :: vals)
    Nothing => Nothing
  Nothing => Nothing
  where
  mVal : Maybe t
  mVal = parsePathParams r


getCaptureName : Path t ts -> String
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
matchPath : { n: Nat } -> {ts: Vect (S n) Type} -> Path t ts -> Vect (S n) String -> { auto allprf : All HasPathParam ts } -> Either String (HVect ts)
matchPath (StaticPath s) [_] { allprf = prf :: restPrf } = Right [()]
matchPath (Capture s t) [seg] { allprf = prf :: restPrf } = case parsePathParams seg  of
  Just val => Right [val]
  Nothing => Left ("Failed to parse path parameter:" ++ s)
matchPath (path :/ restPath) (seg :: segs) { allprf = prf :: restPrf } = case mVal of
  Just val => case matchPath restPath segs of
    Right vals => Right (val :: vals)
    Left err => Left err
  Nothing => Left ("Failed to parse path parameter:" ++ getCaptureName path)
  where
  mVal : Maybe t
  mVal = parsePathParams seg
matchPath _ segs = Left ("unknown path" ++ show segs)


namespace API

  public export
  data API : Type where
    (:/) : (FromJSON req, ToJSON resp) 
       => { ts: Vect (S n) Type} -> (Path t ts) -> { auto prf : All HasPathParam ts } -> Endpoint req resp -> API

record UserId where
  constructor MkUserId
  id : Nat 

implementation HasPathParam UserId where
  parsePathParams s = case parsePositive s of
    Just n => Just (MkUserId n)
    Nothing => Nothing

%runElab derive "UserId" [Show,Eq,ToJSON, FromJSON]

record User where
  constructor MkUser
  id : UserId
  name : String

%runElab derive "User" [Show,Eq,ToJSON, FromJSON]

|||
||| @ts The types of the path parameters.
||| @path The path.
||| @epType The type of the endpoint.
|||
||| Returns the type of the path.
public export
GetPathType : { ts : Vect (S n) Type } -> Path _ ts -> (epType : Type) -> Type
GetPathType (StaticPath _) epType = epType
GetPathType (Capture _ t) epType = t -> epType
GetPathType { ts = t :: ts'} (path :/ restPath) epType = let epType' = GetPathType restPath epType in case path of
  Capture _ t => t -> epType'
  _ => epType'

public export
GetHandlerType : API -> Type
GetHandlerType (path :/ ep) = GetPathType path $ GetEndpointType ep

vals : Vect 3 String
vals = ["users", "2ss", "3dd"]

Tys : Vect 3 Type
Tys = [Bool, Int, String]

p1 : Path () [()]
p1 = StaticPath "users"

p2 : Path UserId [UserId]
p2 = Capture "id" UserId

p3 : Path String [String, UserId, String]
p3 = Capture "users" String :/ p2 :/ Capture "name" String

p4 : Path () [(), UserId, String]
p4 = StaticPath "users" :/ p2 :/ Capture "name" String

ep1 : Endpoint () User
ep1 = Get User

api : API
api = p3 :/ ep1

-- handleType = GetHandlerType api

res = matchPath p3 vals

-- res : Maybe (HVect Tys)
-- res = parsePathParams Tys vals