||| Verbs for HTTP methods
module Pact.API.Verb

import public Pact.WAI.Method
import public Pact.WAI.StatusCode

||| Verb is a record that contains the HTTP method, status code, accept type, and response type
public export
record Verb where
  constructor MkVerb
  ||| HTTP method
  method : Method
  ||| HTTP status code
  status : StatusCode
  ||| Accept type
  accept : Type
  ||| Response type
  response : Type

public export
VerbResponse: Verb -> Type
VerbResponse (MkVerb _ _ _ response) = response

public export
GetEpResultType: Verb -> Type
GetEpResultType (MkVerb _ _ _ response) = response

public export
GetVerbType : (m : Type -> Type) -> Verb -> Type
GetVerbType m (MkVerb _ _ _ response) = m response

public export
GetEndpointType : (m : Type -> Type) -> Verb -> Type
GetEndpointType m (MkVerb _ _ _ response) = m response

--- 200 ---

||| Head verb
public export
Head' : StatusCode -> Type -> Type -> Verb
Head' = MkVerb HEAD 

||| Get verb
public export
Get' : StatusCode -> Type -> Type -> Verb
Get' = MkVerb GET 

||| Post verb
public export
Post' : StatusCode -> Type -> Type -> Verb
Post' = MkVerb POST 

||| Put verb
public export
Put' : StatusCode -> Type -> Type -> Verb
Put' = MkVerb PUT 

||| Delete verb
public export
Delete' : StatusCode -> Type -> Type -> Verb
Delete' = MkVerb DELETE 

||| Patch verb
public export
Patch' : StatusCode -> Type -> Type -> Verb
Patch' = MkVerb PATCH 

||| Head verb with code 200
public export
Head : Type -> Type -> Verb
Head = Head' code_200

||| Get verb with code 200
public export
Get : Type -> Type -> Verb
Get = Get' code_200

||| Post verb with code 201
public export
Post : Type -> Type -> Verb
Post = Post' code_201

||| Put verb with code 200
public export
Put : Type -> Type -> Verb
Put = Put' code_200

||| Delete verb with code 200
public export
Delete : Type -> Type -> Verb
Delete = Delete' code_200

||| Patch verb with code 200
public export
Patch : Type -> Type -> Verb
Patch = Patch' code_200

||| Post verb with code 201 (Created)
public export
PostCreated : Type -> Type -> Verb
PostCreated = Post' code_201

||| Put verb with code 201 (Created)
public export
PutCreated : Type -> Type -> Verb
PutCreated = Put' code_201

||| Get verb with code 202 (Accepted)
public export
GetAccepted : Type -> Type -> Verb
GetAccepted = Get' code_202

||| Post verb with code 202 (Accepted)
public export
PostAccepted : Type -> Type -> Verb
PostAccepted = Post' code_202

||| Delete verb with code 202 (Accepted)
public export
DeleteAccepted : Type -> Type -> Verb
DeleteAccepted = Delete' code_202

||| Patch verb with code 202 (Accepted)
public export
PatchAccepted : Type -> Type -> Verb
PatchAccepted = Patch' code_202

||| Put verb with code 202 (Accepted)
public export
PutAccepted : Type -> Type -> Verb
PutAccepted = Put' code_202

||| Get verb with code 203 (Non-Authoritative)
public export
GetNonAuthoritative : Type -> Type -> Verb
GetNonAuthoritative = Get' code_203

||| Post verb with code 203 (Non-Authoritative)
public export
PostNonAuthoritative : Type -> Type -> Verb
PostNonAuthoritative = Post' code_203

||| Delete verb with code 203 (Non-Authoritative)
public export
DeleteNonAuthoritative : Type -> Type -> Verb
DeleteNonAuthoritative = Delete' code_203

||| Patch verb with code 203 (Non-Authoritative)
public export
PatchNonAuthoritative : Type -> Type -> Verb
PatchNonAuthoritative = Patch' code_203

||| Put verb with code 203 (Non-Authoritative)
public export
PutNonAuthoritative : Type -> Type -> Verb
PutNonAuthoritative = Put' code_203

||| Get verb with code 204 (No Content)
public export
GetNoContent : Type -> Type -> Verb
GetNoContent = Get' code_204

||| Post verb with code 204 (No Content)
public export
PostNoContent : Type -> Type -> Verb
PostNoContent = Post' code_204

||| Delete verb with code 204 (No Content)
public export
DeleteNoContent : Type -> Type -> Verb
DeleteNoContent = Delete' code_204

||| Patch verb with code 204 (No Content)
public export
PatchNoContent : Type -> Type -> Verb
PatchNoContent = Patch' code_204

||| Put verb with code 204 (No Content)
public export
PutNoContent : Type -> Type -> Verb
PutNoContent = Put' code_204

||| Head verb with code 204 (No Content)
public export
HeadNoContent : Type -> Type -> Verb
HeadNoContent = Head' code_204

||| Head verb with code 205 (Reset Content)
public export
HeadResetContent : Type -> Type -> Verb
HeadResetContent = Head' code_205

||| Get verb with code 205 (Reset Content)
public export
GetResetContent : Type -> Type -> Verb
GetResetContent = Get' code_205

||| Post verb with code 205 (Reset Content)
public export
PostResetContent : Type -> Type -> Verb
PostResetContent = Post' code_205

||| Delete verb with code 205 (Reset Content)
public export
DeleteResetContent : Type -> Type -> Verb
DeleteResetContent = Delete' code_205

||| Patch verb with code 205 (Reset Content)
public export
PatchResetContent : Type -> Type -> Verb
PatchResetContent = Patch' code_205

||| Put verb with code 205 (Reset Content)
public export
PutResetContent : Type -> Type -> Verb
PutResetContent = Put' code_205
