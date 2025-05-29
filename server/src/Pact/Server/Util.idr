||| Experimental API definition
module Pact.Server.Util

import Data.String
import Data.Vect
import Data.Vect.Quantifiers
import Data.List1
import JSON

import Pact.WAI
import Pact.API

import Pact.Server.Core


%default total
%default covering


||| Convert an Endpoint to a Method
public export
endpointToMethod : Endpoint req resp -> Method
endpointToMethod HEAD = HEAD
endpointToMethod CONNECT = CONNECT
endpointToMethod (OPTIONS resType) = OPTIONS
endpointToMethod (TRACE resType) = TRACE
endpointToMethod (Get resType) = GET
endpointToMethod (Post reqType resType) = POST
endpointToMethod (Put reqType resType) = PUT
endpointToMethod (Delete reqType resType) = DELETE
endpointToMethod (Patch reqType resType) = PATCH


matchRouteItem : {m : Type -> Type} -> RouteItem m -> Vect n String -> Bool
matchRouteItem (api :=> handler) ss = case matchAPI api ss of
  Right vals => True
  Left err => False


findRouteItem : {m : Type -> Type} -> List (RouteItem m) -> Vect n String -> Maybe (RouteItem m)
findRouteItem [] _ = Nothing
findRouteItem (item :: routes) segs = case matchRouteItem item segs of
    True => Just item
    False => findRouteItem routes segs

strToVect : String -> (n ** Vect n String)
strToVect s = (length list ** fromList list)
  where
  list : List String
  list = filter (/= "") . forget . split (== '/') $ s 

findOnRouter  : {m : Type -> Type} -> Router m -> String -> Maybe (RouteItem m)
findOnRouter (MkRouter routes) s = case strToVect s of
  (n ** segs) => findRouteItem routes segs


applyHandler : (api : API tts) -> (params: HVect tts) -> { auto allprf: All HasPathParam tts} -> (handler: GetHandlerType m api) -> GetEPFromAPI m api
applyHandler ((StaticPath _) :/ ep) [()] handler = handler
applyHandler ((Capture _ t) :/ ep) [param] handler = handler param
applyHandler (((StaticPath _) :/ path') :/ ep) (param :: params) { allprf = prf :: prfs} handler = applyHandler {m} (path' :/ ep) params handler
applyHandler (((Capture _ t) :/ path') :/ ep) (param :: params) { allprf = prf :: prfs} handler = applyHandler {m} (path' :/ ep) params $ handler param

applyHandlerWithRequest : {m : Type -> Type} -> (routeItem : RouteItem m) -> (req : Request) -> Maybe (GetEndpointTypeFromRouteItem m routeItem)
applyHandlerWithRequest (api@(path :/ ep) :=> handler) req = 
  let uri = req.uri
      (_ ** segs) = strToVect uri
      eParams = matchPath path segs
  in case eParams of
    (Right params) => Just $ applyHandler {m} (path :/ ep) params handler
    _ => Nothing

public export
processRequest : Router IO -> Request -> HTTPResponse
processRequest router req = case findOnRouter router req.uri of
  Just routeItem@((:=>) api@(path :/ ep) handler { toJSONProof }) => case applyHandlerWithRequest routeItem req of
    Just ioRes => liftIO ioRes >>= emit . JSONResponse
    Nothing => emit notFoundResponse
  Nothing => emit notFoundResponse