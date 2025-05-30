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
implementation MimeRender PlainTextAccept String where
  mimeRender = id

public export
implementation ToJSON a => MimeRender JSONAccept a where
  mimeRender = encode

public export
implementation MimeRender FormUrlEncodedAccept a where
  mimeRender = ?formUrlEncodedRender

public export
implementation MimeRender OctetStreamAccept a where
  mimeRender = ?octetStreamRender

public export
implementation MimeRender XMLAccept a where
  mimeRender = ?xmlRender

||| Mime unrender a value from a string.
public export
interface Accept ctype => MimeUnrender ctype a where
  ||| Unrender a value from a string.
  mimeUnrender : String -> Either String a

--- Implementations

public export
implementation MimeUnrender PlainTextAccept String where
  mimeUnrender = Right


public export
implementation FromJSON a => MimeUnrender JSONAccept a where
  mimeUnrender = either (Left . show) Right . decode

public export
implementation MimeUnrender FormUrlEncodedAccept a where
  mimeUnrender = ?formUrlEncodedUnrender

public export
implementation MimeUnrender OctetStreamAccept a where
  mimeUnrender = ?octetStreamUnrender

public export
implementation MimeUnrender XMLAccept a where
  mimeUnrender = ?xmlUnrender

