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
getPath : (path: String) -> Result String
getPath p = do
  url' <- pure $ "http://localhost:2222" ++ p
  url <- MkEitherT . pure . url_from_string $ url'
  get url

public export
GetGenerateLinkFunType : Component _ _ _ -> Type
GetGenerateLinkFunType (StaticPath path) = Result String
GetGenerateLinkFunType (Capture name ty) = ty -> Result String
GetGenerateLinkFunType (ReqBody _) = Result String
GetGenerateLinkFunType (StaticPath path :/ rest) = GetGenerateLinkFunType rest
GetGenerateLinkFunType (Capture name ty :/ rest) = ty -> GetGenerateLinkFunType rest
GetGenerateLinkFunType (ReqBody _ :/ rest) = GetGenerateLinkFunType rest

public export
GetGenerateLinkByAPI : API ts -> Type
GetGenerateLinkByAPI (path :> _) = GetGenerateLinkFunType path

generateLink : (comp: Component t ts r) -> {auto allprf : All ToHttpApiData ts} -> (acc: String) -> GetGenerateLinkFunType comp
generateLink (StaticPath path) acc = getPath "\{acc}/\{path}"
generateLink (Capture name ty) {allprf = prf :: restPrf} acc = (\x: ty => getPath "\{acc}/\{toUrlPiece x}")
generateLink (ReqBody _) acc = getPath acc
generateLink (StaticPath path :/ rest) {allprf = prf :: restPrf} acc = generateLink rest $ "\{acc}/\{path}"
generateLink (Capture name ty :/ rest) {allprf = prf :: restPrf} acc = (\x: ty => generateLink rest $ "\{acc}/\{toUrlPiece x}")
generateLink (ReqBody _ :/ _) acc = assert_total $ idris_crash "ReqBody is not supported"

public export
generateLinkByAPI : (api: API ts) -> {auto allprf : All ToHttpApiData ts} -> GetGenerateLinkByAPI api
generateLinkByAPI (path :> _) = generateLink path ""

