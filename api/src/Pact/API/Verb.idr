||| Verbs for HTTP methods
module Pact.API.Verb

import Pact.WAI.Method
import Pact.WAI.StatusCode

record Verb where
  constructor MkVerb
  ||| HTTP method
  method : Method
  ||| HTTP status code
  status : StatusCode
  ||| Response type
  response : Type


public export
GetEpResultType: Verb -> Type
GetEpResultType (MkVerb method status response) = response

public export
GetVerbType : (m : Type -> Type) -> Verb -> Type
GetVerbType m (MkVerb method status response) = m response


--- 200 ---

||| Head verb
public export
Head' : StatusCode -> Type -> Verb
Head' = MkVerb HEAD 

||| Get verb
public export
Get' : StatusCode -> Type -> Verb
Get' = MkVerb GET 

||| Post verb
public export
Post' : StatusCode -> Type -> Verb
Post' = MkVerb POST 

||| Put verb
public export
Put' : StatusCode -> Type -> Verb
Put' = MkVerb PUT 

||| Delete verb
public export
Delete' : StatusCode -> Type -> Verb
Delete' = MkVerb DELETE 

||| Patch verb
public export
Patch' : StatusCode -> Type -> Verb
Patch' = MkVerb PATCH 

||| Head verb with code 200
public export
Head : Type -> Verb
Head = Head' code_200

||| Get verb with code 200
public export
Get : Type -> Verb
Get = Get' code_200

||| Post verb with code 201
public export
Post : Type -> Verb
Post = Post' code_201

||| Put verb with code 200
public export
Put : Type -> Verb
Put = Put' code_200

||| Delete verb with code 200
public export
Delete : Type -> Verb
Delete = Delete' code_200

||| Patch verb with code 200
public export
Patch : Type -> Verb
Patch = Patch' code_200

||| Post verb with code 201 (Created)
public export
PostCreated : Type -> Verb
PostCreated = Post' code_201

||| Put verb with code 201 (Created)
public export
PutCreated : Type -> Verb
PutCreated = Put' code_201

||| Get verb with code 202 (Accepted)
public export
GetAccepted : Type -> Verb
GetAccepted = Get' code_202

||| Post verb with code 202 (Accepted)
public export
PostAccepted : Type -> Verb
PostAccepted = Post' code_202

||| Delete verb with code 202 (Accepted)
public export
DeleteAccepted : Type -> Verb
DeleteAccepted = Delete' code_202

||| Patch verb with code 202 (Accepted)
public export
PatchAccepted : Type -> Verb
PatchAccepted = Patch' code_202

||| Put verb with code 202 (Accepted)
public export
PutAccepted : Type -> Verb
PutAccepted = Put' code_202

||| Get verb with code 203 (Non-Authoritative)
public export
GetNonAuthoritative : Type -> Verb
GetNonAuthoritative = Get' code_203

||| Post verb with code 203 (Non-Authoritative)
public export
PostNonAuthoritative : Type -> Verb
PostNonAuthoritative = Post' code_203

||| Delete verb with code 203 (Non-Authoritative)
public export
DeleteNonAuthoritative : Type -> Verb
DeleteNonAuthoritative = Delete' code_203

||| Patch verb with code 203 (Non-Authoritative)
public export
PatchNonAuthoritative : Type -> Verb
PatchNonAuthoritative = Patch' code_203

||| Put verb with code 203 (Non-Authoritative)
public export
PutNonAuthoritative : Type -> Verb
PutNonAuthoritative = Put' code_203

||| Get verb with code 204 (No Content)
public export
GetNoContent : Type -> Verb
GetNoContent = Get' code_204

||| Post verb with code 204 (No Content)
public export
PostNoContent : Type -> Verb
PostNoContent = Post' code_204

||| Delete verb with code 204 (No Content)
public export
DeleteNoContent : Type -> Verb
DeleteNoContent = Delete' code_204

||| Patch verb with code 204 (No Content)
public export
PatchNoContent : Type -> Verb
PatchNoContent = Patch' code_204

||| Put verb with code 204 (No Content)
public export
PutNoContent : Type -> Verb
PutNoContent = Put' code_204

||| Head verb with code 204 (No Content)
public export
HeadNoContent : Type -> Verb
HeadNoContent = Head' code_204

||| Head verb with code 205 (Reset Content)
public export
HeadResetContent : Type -> Verb
HeadResetContent = Head' code_205

||| Get verb with code 205 (Reset Content)
public export
GetResetContent : Type -> Verb
GetResetContent = Get' code_205

||| Post verb with code 205 (Reset Content)
public export
PostResetContent : Type -> Verb
PostResetContent = Post' code_205

||| Delete verb with code 205 (Reset Content)
public export
DeleteResetContent : Type -> Verb
DeleteResetContent = Delete' code_205

||| Patch verb with code 205 (Reset Content)
public export
PatchResetContent : Type -> Verb
PatchResetContent = Patch' code_205

||| Put verb with code 205 (Reset Content)
public export
PutResetContent : Type -> Verb
PutResetContent = Put' code_205
