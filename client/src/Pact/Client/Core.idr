module Pact.Client.Core

import Pact.API.Component
import Pact.API

import Data.Vect
import Data.Vect.Quantifiers

import Control.Monad.Either


import Network.HTTP
import Utils.String
import public Network.HTTP.URL
import Data.Either
import Pact.API.Verb
import Pact.API.MimeRender

%hide  Utils.Streaming.infixl.(:>)

public export
Result : Type -> Type
Result a = EitherT String IO a

map_error : Functor m => (e -> e') -> EitherT e m a -> EitherT e' m a
map_error f = bimapEitherT f id


with_client : {e : _} -> IO (HttpClient e) -> (HttpClient e -> EitherT (HttpError e) IO a) -> EitherT (HttpError e) IO a
with_client client f = MkEitherT $ do
  c <- client
  Right ok <- runEitherT (f c)
  | Left err => close c *> pure (Left err)
  close c
  pure (Right ok)

export
get : (url : URL) -> Result String
get url = map_error show $ with_client {e=()} new_client_default $ \client => do
  (response, content) <- request client GET url [] ()
  content <- toList_ content
  case utf8_pack content of
    Nothing => pure "Failed to decode content"
    Just content => pure content

export
getPath : (path: String) -> (accept: Type) -> (t: Type) -> {auto prf: MimeUnrender accept t} -> Result t
getPath p accept resType {prf} = do
  url' <- pure $ "http://localhost:2222" ++ p
  url <- MkEitherT . pure . url_from_string $ url'
  content <- get url
  case mimeUnrender {ctype = accept} {a = resType} content of
    Left err => throwE err
    Right res => pure res

public export
GetGenerateLinkFunType : Component _ _ _ -> Type -> Type
GetGenerateLinkFunType (StaticPath path) resType = Result resType
GetGenerateLinkFunType (Capture name ty) resType = ty -> Result resType
GetGenerateLinkFunType (ReqBody _) resType = Result resType
GetGenerateLinkFunType (StaticPath path :/ rest) resType = GetGenerateLinkFunType rest resType
GetGenerateLinkFunType (Capture name ty :/ rest) resType = ty -> GetGenerateLinkFunType rest resType
GetGenerateLinkFunType (ReqBody _ :/ rest) resType = GetGenerateLinkFunType rest resType

public export
GetGenerateLinkByAPI : API -> Type
GetGenerateLinkByAPI (path :> verb) = GetGenerateLinkFunType path (VerbResponse verb)

generateLink : (comp: Component t ts r) -> {auto allprf : All ToHttpApiData ts} -> (acc: String) -> (accept: Type) -> (resType: Type) -> {auto prf: MimeUnrender accept resType} -> GetGenerateLinkFunType comp resType
generateLink (StaticPath path) acc accept resType = getPath "\{acc}/\{path}" accept resType
generateLink (Capture name ty) {allprf = prf :: restPrf} acc accept resType = (\x: ty => getPath "\{acc}/\{toUrlPiece x}" accept resType)
generateLink (ReqBody _) acc accept resType = getPath acc accept resType
generateLink (StaticPath path :/ rest) {allprf = prf :: restPrf} acc accept resType = generateLink rest  "\{acc}/\{path}" accept resType
generateLink (Capture name ty :/ rest) {allprf = prf :: restPrf} acc accept resType = (\x: ty => generateLink rest "\{acc}/\{toUrlPiece x}" accept resType)
generateLink (ReqBody _ :/ _) acc accept resType = assert_total $ idris_crash "ReqBody is not supported"

public export
generateLinkByAPI : (api: API) -> {allprf : All ToHttpApiData api.types} -> {verbPrf: MimeUnrender (VerbAccept api.verb) (VerbResponse api.verb)} -> GetGenerateLinkByAPI api
generateLinkByAPI (path :> verb) = generateLink path "" (VerbAccept verb) (VerbResponse verb)

