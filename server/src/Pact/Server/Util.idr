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


||| Match an API against a list of path segments.
|||
||| @api The API to match.
||| @segs The list of path segments.
|||
||| Returns a list of the parsed path parameters.
public export
matchAPI : API ts -> Vect m String -> Request -> Either String (HVect ts)
matchAPI (path :> ep) segs req = if req.method == ep.method 
                          then matchPath path segs 
                          else Left "Method not allowed"

matchRouteItem : {m : Type -> Type} -> RouteItem m -> Vect n String -> Request -> Bool
matchRouteItem (api :=> handler) ss req = case matchAPI api ss req of
  Right vals => True
  Left err => False


findRouteItem : {m : Type -> Type} -> List (RouteItem m) -> Vect n String -> Request -> Maybe (RouteItem m)
findRouteItem [] _ req = Nothing
findRouteItem (item :: routes) segs req = case matchRouteItem item segs req of
    True => Just item
    False => findRouteItem routes segs req

strToVect : String -> (n ** Vect n String)
strToVect s = (length list ** fromList list)
  where
  list : List String
  list = filter (/= "") . forget . split (== '/') $ s 

findOnRouter  : {m : Type -> Type} -> Router m -> Request -> Maybe (RouteItem m)
findOnRouter (MkRouter routes) req = case strToVect req.uri of
  (n ** segs) => findRouteItem routes segs req

applyHandler : (api : API tts) -> (params: HVect tts) -> { auto allprf: All HasPathParam tts} -> (handler: GetHandlerType m api) -> GetEPFromAPI m api
applyHandler ((StaticPath _) :> ep) [()] handler = handler
applyHandler ((Capture _ t) :> ep) [param] handler = handler param
applyHandler (((StaticPath _) :/ path') :> ep) (param :: params) { allprf = prf :: prfs} handler = applyHandler {m} (path' :> ep) params handler
applyHandler (((Capture _ t) :/ path') :> ep) (param :: params) { allprf = prf :: prfs} handler = applyHandler {m} (path' :> ep) params $ handler param

applyHandlerWithRequest : {m : Type -> Type} -> (routeItem : RouteItem m) -> (req : Request) -> Maybe (GetEndpointTypeFromRouteItem m routeItem)
applyHandlerWithRequest (api@(path :> ep) :=> handler) req = 
  let uri = req.uri
      (_ ** segs) = strToVect uri
      eParams = matchPath path segs
  in case eParams of
    (Right params) => Just $ applyHandler {m} (path :> ep) params handler
    _ => Nothing


emitResponse : (v: Verb) -> MimeRender (VerbAccept v) (VerbResponse v) => VerbResponse v -> HTTPResponse
emitResponse (MkVerb _ code accept' response') res = emit $ MkResponse code headers (Just bodyStr)
  where
  bodyStr : String
  bodyStr = mimeRender { ctype = accept' } res
  bodyLen : String
  bodyLen =  show $ length bodyStr
  contentTypeStr : String
  contentTypeStr = show $ contentType { ctype = accept' }
  headers : Headers
  headers = if code == code_204 then emptyHeaders else fromList [("Content-Type", contentTypeStr ), ("Content-Length", bodyLen)]

public export
processRequest : Router IO -> Request -> HTTPResponse
processRequest router req = case findOnRouter router req of
  Just routeItem@((:=>) api@(path :> verb) handler { mimeRenderProof }) => case applyHandlerWithRequest routeItem req of
    Just ioRes => liftIO ioRes >>= emitResponse verb
    Nothing => throw InvalidRequest
  Nothing => throw InvalidRequest