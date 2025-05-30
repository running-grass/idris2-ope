||| Accept a content type
module Pact.API.Accept

import public Data.Mime.Apache.Model
import Data.Mime.Apache

||| Plain text content type
public export
data PlainTextAccept : Type where

||| JSON content type
public export
data JSONAccept : Type where

||| XMLAccept content type
public export
data XMLAccept : Type where

||| Form URL encoded content type
public export
data FormUrlEncodedAccept : Type where

||| Octet stream content type
public export
data OctetStreamAccept : Type where

||| Accept a content type.
public export
interface Accept ctype where
  ||| Get the content type.
  contentType : Mime

--- Implementations
public export
implementation Accept PlainTextAccept where
  contentType  = TEXT_PLAIN

public export
implementation Accept JSONAccept where
  contentType  = APPLICATION_JSON 

public export
implementation Accept FormUrlEncodedAccept where
  contentType  = APPLICATION_X_WWW_FORM_URLENCODED

public export
implementation Accept OctetStreamAccept where
  contentType  = APPLICATION_OCTET_STREAM

public export
implementation Accept XMLAccept where
  contentType  = APPLICATION_XML