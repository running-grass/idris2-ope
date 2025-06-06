module Pact.Client.Core

import Pact.API.Component
import Pact.API

import Data.Vect
import Data.Vect.Quantifiers

public export
GetGenerateLinkFunType : Component _ _ _ -> Type
GetGenerateLinkFunType (StaticPath path) = String
GetGenerateLinkFunType (Capture name ty) = ty -> String
GetGenerateLinkFunType (ReqBody _) = String
GetGenerateLinkFunType (StaticPath path :/ rest) = GetGenerateLinkFunType rest
GetGenerateLinkFunType (Capture name ty :/ rest) = ty -> GetGenerateLinkFunType rest
GetGenerateLinkFunType (ReqBody _ :/ rest) = GetGenerateLinkFunType rest

public export
GetGenerateLinkByAPI : API ts -> Type
GetGenerateLinkByAPI (path :> _) = GetGenerateLinkFunType path

generateLink : (comp: Component t ts r) -> {auto allprf : All ToHttpApiData ts} -> (acc: String) -> GetGenerateLinkFunType comp
generateLink (StaticPath path) acc = "\{acc}/\{path}"
generateLink (Capture name ty) {allprf = prf :: restPrf} acc = (\x: ty => "\{acc}/\{toUrlPiece x}")
generateLink (ReqBody _) acc = acc
generateLink (StaticPath path :/ rest) {allprf = prf :: restPrf} acc = generateLink rest $ "\{acc}/\{path}"
generateLink (Capture name ty :/ rest) {allprf = prf :: restPrf} acc = (\x: ty => generateLink rest $ "\{acc}/\{toUrlPiece x}")
generateLink (ReqBody _ :/ _) acc = assert_total $ idris_crash "ReqBody is not supported"

public export
generateLinkByAPI : (api: API ts) -> {auto allprf : All ToHttpApiData ts} -> GetGenerateLinkByAPI api
generateLinkByAPI (path :> _) = generateLink path ""

