module Ope.API.Util

import Ope.API.Core
import Data.Vect
import Data.Vect.Quantifiers

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


vals : Vect 3 String
vals = ["1", "2", "3"]

Tys : Vect 3 Type
Tys = [Bool, Int, String]

res : Maybe (HVect Tys)
res = parsePathParams Tys vals