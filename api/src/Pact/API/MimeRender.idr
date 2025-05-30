||| Mime render and unrender.
module Pact.API.MimeRender

import Pact.API.Accept
import JSON.FromJSON
import JSON.ToJSON

||| Mime render a value to a string.
public export
interface Accept ctype => MimeRender ctype a where
  ||| Render a value to a string.
  mimeRender : a -> String

--- Implementations

public export
implementation MimeRender PlainText String where
  mimeRender = id

public export
implementation ToJSON a => MimeRender JSON a where
  mimeRender = encode

public export
implementation MimeRender FormUrlEncoded a where
  mimeRender = ?formUrlEncodedRender

public export
implementation MimeRender OctetStream a where
  mimeRender = ?octetStreamRender

public export
implementation MimeRender XML a where
  mimeRender = ?xmlRender

||| Mime unrender a value from a string.
public export
interface Accept ctype => MimeUnrender ctype a where
  ||| Unrender a value from a string.
  mimeUnrender : String -> Either String a

--- Implementations

public export
implementation MimeUnrender PlainText String where
  mimeUnrender = Right


public export
implementation FromJSON a => MimeUnrender JSON a where
  mimeUnrender = either (Left . show) Right . decode

public export
implementation MimeUnrender FormUrlEncoded a where
  mimeUnrender = ?formUrlEncodedUnrender

public export
implementation MimeUnrender OctetStream a where
  mimeUnrender = ?octetStreamUnrender

public export
implementation MimeUnrender XML a where
  mimeUnrender = ?xmlUnrender

