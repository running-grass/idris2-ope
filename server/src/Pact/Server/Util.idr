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
import FS.Core

import FS.Posix
import FS.Socket

import IO.Async.Loop.Posix
import IO.Async.Loop.Epoll
import FS.Concurrent

import Control.Monad.Error.Either


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

applyHandler : (api : API tts) -> (params: HVect tts) -> Lazy (Either DecodingErr (ApiReqBody api)) -> { auto allprf: All HasPathParam tts} -> (handler: GetHandlerType m api) -> Either String (GetEPFromAPI m api)
applyHandler ((StaticPath _) :> ep) [()] _ handler = Right handler
applyHandler ((Capture _ t) :> ep) [param] _ handler = Right $ handler param
applyHandler ((ReqBody reqType) :> ep) _ (Right reqBody) handler = Right $ handler reqBody
applyHandler ((ReqBody reqType) :> ep) _ (Left err) handler = Left $ "parse error: \{show err}"
applyHandler (((StaticPath _) :/ path') :> ep) (param :: params) reqBody { allprf = prf :: prfs} handler = applyHandler {m} (path' :> ep) params reqBody handler
applyHandler (((Capture _ t) :/ path') :> ep) (param :: params) reqBody { allprf = prf :: prfs} handler = applyHandler {m} (path' :> ep) params reqBody $ handler param
-- applyHandler (((ReqBody reqType) :/ path') :> ep) (_ :: params) reqBody@(Just reqBody')  { allprf = prf :: prfs} handler = applyHandler {m} (path' :> ep) params reqBody $ handler reqBody'
applyHandler _ _ _ _ = Left "Invalid request"

applyHandlerWithRequest : {m : Type -> Type} -> (routeItem : RouteItem m) -> (req : Request) -> String -> Either String (GetEndpointTypeFromRouteItem m routeItem)
applyHandlerWithRequest ((:=>) api@(path :> ep) handler { mimeRenderProof } { reqBodyProof }) req reqBody = 
  let uri = req.uri
      (_ ** segs) = strToVect uri
      eParams = matchPath path segs
      reqBody' : Either DecodingErr (PathReqBody path) = delay $ decode reqBody
  in case (eParams) of
    (Right params) => applyHandler {m} (path :> ep) params reqBody'  handler
    (Left err) => Left $ "\{err}"


emitResponse : (v: Verb) -> MimeRender (VerbAccept v) (VerbResponse v)  =>  VerbResponse v -> HTTPResponse
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


accumAsString : Pull f ByteString es () -> Pull f q es String 
accumAsString p =
     foldGet (:<) [<] p
  |> map (toString . ByteString.fastConcat . (<>> []))

public export
serve : {m : Type -> Type} -> Hoistable m => Router m -> Request -> HTTPResponse
serve router req = accumAsString req.body >>= hand
  where
  hand : String -> HTTPResponse
  hand reqBody = case findOnRouter router req of
    Just routeItem@((:=>) api@(path :> verb) handler { mimeRenderProof } { reqBodyProof }) => case (applyHandlerWithRequest routeItem req reqBody) of
      Right ioRes => (liftIO . runEitherT . hoist {m} $ ioRes ) >>= \case 
        Right res => emitResponse verb res
        Left err => throw err
      Left err => throw InvalidRequest
    Nothing => throw InvalidRequest