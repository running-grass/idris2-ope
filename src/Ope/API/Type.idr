module Ope.API.Type

import Data.Vect
import Data.Vect.Quantifiers

public export
infixr 5 :>

data API : ( ts: Vect n Type) -> Type -> Type where
  Static : String -> API [] a
  Capture : String -> (c: Type) -> API [c] a
  Get : (resp : Type) -> API [] resp
  (:>) : API tl a -> API tr a -> API (tl ++ tr) a

getCaptureTypes : {ts : Vect n Type} -> API ts a -> Vect n Type
getCaptureTypes _ = ts

isValidAPI : {ts : Vect n Type} -> {a : Type} -> API ts a
            -> Bool
isValidAPI (Get _) = True
isValidAPI (_ :> (Get _)) = True
isValidAPI _ = False

record User where
  constructor MkUser
  name : String
  age : Nat

Handler : {ts : Vect n Type} -> { r : Type } 
          -> ( api : API ts r )
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
  api : API ts r
  handler: {auto 0 prf : isValidAPI api = True} -> Handler api

data Server : Type where
  Nil: Server
  (::) : Route ts r -> Server -> Server


api11 : API [] a
api11 = Static "users" 

-- api12 : API [Nat] Int
api12 = Capture "id" Nat :> Get Int

handler12 : Handler Type.api12
handler12 id = 100

api13 : API [String] a
api13 = Capture "name" String

api14 : API [] String
api14 = Get String

api21 : API [Nat] Int
api21 = api11 :> api12


api22 : API [String] String
api22 = api13 :> api14

handler22 : Handler Type.api22
handler22 name = "Hello, " ++ name

api23 : API [] User
api23 = Get User

route1 = api12 :=> handler12
route2 = api22 :=> handler22

server1 : Server
server1 = [route1, route2]
