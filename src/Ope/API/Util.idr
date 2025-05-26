module Ope.API.Util

import Ope.API.Core
import Data.Vect
import Data.Vect.Quantifiers

data Path : Type -> Vect (S n) Type -> Type where
  ||| Static path segment, e.g. "users"
  StaticPath : String -> Path () [()]
  ||| Captured path segment, e.g. ":id"
  Capture : String -> (a : Type) -> Path a [a]

  (:/) : Path tl _ -> Path _ tsr -> Path tl (tl :: tsr)

||| Parse a vector of path parameters into a vector of types.
|||
||| @pts The types of the path parameters.
||| @allprf A proof that all the path parameters are path parameters.
||| @raw The raw path parameters.
|||
||| Returns a vector of the parsed path parameters.
public export
parsePathParams : { n : Nat } -> ( pts: Vect n Type ) ->  {auto allprf :  All PathParams pts } -> (raw : Vect n String) -> Maybe (HVect pts)
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
matchPath : { n: Nat } -> {ts: Vect (S n) Type} -> Path t ts -> Vect (S n) String -> { auto allprf : All PathParams ts } -> Either String (HVect ts)
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

vals : Vect 3 String
vals = ["users", "2ss", "3dd"]

Tys : Vect 3 Type
Tys = [Bool, Int, String]

p1 : Path () [()]
p1 = StaticPath "users"

p2 : Path Int [Int]
p2 = Capture "id" Int

p3 = StaticPath "users" :/ Capture "id" Int :/ Capture "name" String

res = matchPath p3 vals

-- res : Maybe (HVect Tys)
-- res = parsePathParams Tys vals