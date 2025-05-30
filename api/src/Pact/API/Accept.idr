||| Accept a content type
module Pact.API.Accept

import Data.Mime.Apache

||| Plain text content type
public export
data PlainText : Type where

||| JSON content type
public export
data JSON : Type where

||| XML content type
public export
data XML : Type where

||| Form URL encoded content type
public export
data FormUrlEncoded : Type where

||| Octet stream content type
public export
data OctetStream : Type where

||| Accept a content type.
public export
interface Accept ctype where
  ||| Get the content type.
  contentType : ctype -> Mime

--- Implementations
public export
implementation Accept PlainText where
  contentType _ = TEXT_PLAIN

public export
implementation Accept JSON where
  contentType _ = APPLICATION_JSON 

public export
implementation Accept FormUrlEncoded where
  contentType _ = APPLICATION_X_WWW_FORM_URLENCODED

public export
implementation Accept OctetStream where
  contentType _ = APPLICATION_OCTET_STREAM

public export
implementation Accept XML where
  contentType _ = APPLICATION_XML