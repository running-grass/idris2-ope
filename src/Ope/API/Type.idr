module Ope.API.Type

import Data.Vect
import Data.Vect.Quantifiers

public export
infixr 5 :>

-- data API : ( ts: Vect n Type) -> Type -> Type where
--   Static : String -> API [] a
--   Capture : String -> (c: Type) -> API [c] a
--   Get : (resp : Type) -> API [] resp
--   (:>) : API tl a -> API tr a -> API (tl ++ tr) a


data API : (len: Nat) -> ( ts: Vect n Type) -> Type -> Type where
  Get : (resp : Type) -> API 0 [] resp
  Static : String -> API 1 [] a
  Capture : String -> (c: Type) -> API 1 [c] a
  (:>) : API 1 tl a -> API len tr a -> API (1 + len) (tl ++ tr) a

getCaptureTypes : {ts : Vect n Type} -> API len ts a -> Vect n Type
getCaptureTypes _ = ts

isValidAPI : {ts : Vect n Type} -> {a : Type} -> API len ts a -> Bool
isValidAPI (Get _) = True
isValidAPI (_ :> (Get _)) = True
isValidAPI _ = False

record User where
  constructor MkUser
  name : String
  age : Nat

Handler : {ts : Vect n Type} -> { r : Type } 
          -> ( api : API len ts r )
          -> {auto 0 prf : isValidAPI api = True} 
          -> Type
Handler _ = getHandlerType ts
  where
  getHandlerType : Vect m Type -> Type
  getHandlerType [] = r
  getHandlerType (c :: cs) = c -> getHandlerType cs

infixr 4 :=>

record Route (ts : Vect n Type) (r : Type) where
  constructor (:=>)
  api :  API len ts r
  handler: { auto prf1 : Show r } -> { auto prf2 : All FromString ts } -> { auto 0 prf : isValidAPI api = True} -> Handler api

data Server : Type where
  Nil: Server
  (::) : Route ts r -> Server -> Server


api11 : API 1 [] a
api11 = Static "users" 

-- api12 : API [Nat] Int
api12 = Capture "id" Nat :> Get Int

handler12 : Handler Type.api12
handler12 id = 100

api13 : API 1 [String] a
api13 = Capture "name" String

api14 : API 0 [] String
api14 = Get String

api21 : API 2 [Nat] Int
api21 = api11 :> api12


api22 : API 2 [String, Nat] String
api22 = api13 :> Capture "age" Nat :> api14

-- handler22 : Handler Type.api22
handler22 name age = "Hello, " ++ name ++ " " 

api23 : API 0 [] User
api23 = Get User

route12 = api12 :=> handler12
route22 = api22 :=> handler22

server1 : Server
server1 = [route12, route22]

ParamsType : {ts : Vect n Type} -> {r : Type} -> Route ts r -> Type
ParamsType _ = HVect ts

Pt22 = ParamsType Type.route22

p22 : Pt22
p22 = ["John", 42]

matchRoutes : String -> Server -> Maybe (Route ts r)
matchRoutes _ [] = Nothing
matchRoutes s (r :: rs) = case matchRoutes s rs of
  Just r' => Just r'
  Nothing => case matchRoutes s rs of
    Just r' => Just r'
    Nothing => Nothing

matchRoute : { ts : Vect n Type} -> {r : Type} -> List String -> ( route : Route ts r ) -> Maybe (ParamsType route)
matchRoute xs (api :=> handler) = ?tt
 where
  matchRoute' : {vs : Vect m Type} -> {hs : Vect n Type} -> { auto 0 prf : ts = ms + ns } ->  List String -> vs -> HVect hs

  -- where
  -- matchRoute' : List String -> Vect m Type -> Maybe (StringParams route)




