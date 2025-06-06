||| HTTP status codes
module Pact.WAI.StatusCode

import Derive.Prelude
import Data.Nat
import Data.Nat.Order

%language ElabReflection
%default total

||| A HTTP status code.
|||
||| The status code is a 3-digit number, the first digit of which
||| indicates the class of status code.
|||
public export
record StatusCode where
  constructor MkStatusCode
  code : Nat
  { auto 0 gte100: 100 <= code = True }
  { auto 0 lte599: code <= 599 = True }

%runElab derive "StatusCode" [Show,Eq,Ord]

public export
implementation Interpolation StatusCode where
  interpolate (MkStatusCode code) = show code

public export
ok : StatusCode
ok = MkStatusCode 200

public export
badRequest : StatusCode
badRequest = MkStatusCode 400

public export
unauthorized : StatusCode
unauthorized = MkStatusCode 401

public export
forbidden : StatusCode
forbidden = MkStatusCode 403

public export
notFound : StatusCode
notFound = MkStatusCode 404

public export
internalServerError : StatusCode
internalServerError = MkStatusCode 500


||| 100 - Continue
public export
code_100 : StatusCode
code_100 = MkStatusCode 100

||| 101 - Switching Protocols
public export
code_101 : StatusCode
code_101 = MkStatusCode 101

||| 102 - Processing
public export
code_102 : StatusCode
code_102 = MkStatusCode 102

||| 103 - Early Hints
public export
code_103 : StatusCode
code_103 = MkStatusCode 103


||| 200 - OK
public export
code_200 : StatusCode
code_200 = MkStatusCode 200

||| 201 - Created
public export
code_201 : StatusCode
code_201 = MkStatusCode 201

||| 202 - Accepted
public export
code_202 : StatusCode
code_202 = MkStatusCode 202

||| 203 - Non-Authoritative Information 
public export
code_203 : StatusCode
code_203 = MkStatusCode 203

||| 204 - No Content
public export
code_204 : StatusCode
code_204 = MkStatusCode 204

||| 205 - Reset Content
public export
code_205 : StatusCode
code_205 = MkStatusCode 205

||| 206 - Partial Content
public export
code_206 : StatusCode
code_206 = MkStatusCode 206

||| 207 - Multi-Status
public export
code_207 : StatusCode
code_207 = MkStatusCode 207

||| 208 - Already Reported
public export
code_208 : StatusCode
code_208 = MkStatusCode 208

||| 226 - IM Used
public export
code_226 : StatusCode
code_226 = MkStatusCode 226

||| 300 - Multiple Choices
public export
code_300 : StatusCode
code_300 = MkStatusCode 300

||| 301 - Moved Permanently
public export
code_301 : StatusCode
code_301 = MkStatusCode 301

||| 302 - Found
public export
code_302 : StatusCode
code_302 = MkStatusCode 302

||| 303 - See Other
public export
code_303 : StatusCode
code_303 = MkStatusCode 303

||| 304 - Not Modified
public export
code_304 : StatusCode
code_304 = MkStatusCode 304

||| 305 - Use Proxy
public export
code_305 : StatusCode
code_305 = MkStatusCode 305

||| 307 - Temporary Redirect
public export
code_307 : StatusCode
code_307 = MkStatusCode 307

||| 308 - Permanent Redirect
public export
code_308 : StatusCode
code_308 = MkStatusCode 308

||| 400 - Bad Request
public export
code_400 : StatusCode
code_400 = MkStatusCode 400

||| 401 - Unauthorized
public export
code_401 : StatusCode
code_401 = MkStatusCode 401

||| 403 - Forbidden
public export
code_403 : StatusCode
code_403 = MkStatusCode 403

||| 404 - Not Found
public export
code_404 : StatusCode
code_404 = MkStatusCode 404

||| 405 - Method Not Allowed
public export
code_405 : StatusCode
code_405 = MkStatusCode 405

||| 406 - Not Acceptable
public export
code_406 : StatusCode
code_406 = MkStatusCode 406

||| 407 - Proxy Authentication Required
public export
code_407 : StatusCode
code_407 = MkStatusCode 407

||| 408 - Request Timeout
public export
code_408 : StatusCode
code_408 = MkStatusCode 408

||| 409 - Conflict
public export
code_409 : StatusCode
code_409 = MkStatusCode 409

||| 410 - Gone
public export
code_410 : StatusCode
code_410 = MkStatusCode 410

||| 411 - Length Required
code_411 : StatusCode
code_411 = MkStatusCode 411

||| 412 - Precondition Failed
public export
code_412 : StatusCode
code_412 = MkStatusCode 412

||| 413 - Payload Too Large
public export
code_413 : StatusCode
code_413 = MkStatusCode 413

||| 414 - URI Too Long
public export
code_414 : StatusCode
code_414 = MkStatusCode 414

||| 415 - Unsupported Media Type
public export
code_415 : StatusCode
code_415 = MkStatusCode 415

||| 416 - Range Not Satisfiable
public export
code_416 : StatusCode
code_416 = MkStatusCode 416

||| 417 - Expectation Failed
public export
code_417 : StatusCode
code_417 = MkStatusCode 417

||| 418 - I'm a teapot
public export
code_418 : StatusCode
code_418 = MkStatusCode 418

||| 421 - Misdirected Request
public export
code_421 : StatusCode
code_421 = MkStatusCode 421

||| 422 - Unprocessable Entity
public export
code_422 : StatusCode
code_422 = MkStatusCode 422

||| 423 - Locked
public export
code_423 : StatusCode
code_423 = MkStatusCode 423

||| 424 - Failed Dependency
public export
code_424 : StatusCode
code_424 = MkStatusCode 424

||| 425 - Too Early
public export
code_425 : StatusCode
code_425 = MkStatusCode 425

||| 426 - Upgrade Required
public export
code_426 : StatusCode
code_426 = MkStatusCode 426

||| 428 - Precondition Required
public export
code_428 : StatusCode
code_428 = MkStatusCode 428

||| 429 - Too Many Requests
public export
code_429 : StatusCode
code_429 = MkStatusCode 429

||| 431 - Request Header Fields Too Large
public export
code_431 : StatusCode
code_431 = MkStatusCode 431

||| 451 - Unavailable For Legal Reasons
public export
code_451 : StatusCode
code_451 = MkStatusCode 451

||| 500 - Internal Server Error
public export
code_500 : StatusCode
code_500 = MkStatusCode 500

||| 501 - Not Implemented
public export
code_501 : StatusCode
code_501 = MkStatusCode 501

||| 502 - Bad Gateway
public export
code_502 : StatusCode
code_502 = MkStatusCode 502

||| 503 - Service Unavailable
public export
code_503 : StatusCode
code_503 = MkStatusCode 503

||| 504 - Gateway Timeout
public export
code_504 : StatusCode
code_504 = MkStatusCode 504

||| 505 - HTTP Version Not Supported
public export
code_505 : StatusCode
code_505 = MkStatusCode 505

||| 506 - Variant Also Negotiates
public export
code_506 : StatusCode
code_506 = MkStatusCode 506

||| 507 - Insufficient Storage
public export
code_507 : StatusCode
code_507 = MkStatusCode 507

||| 508 - Loop Detected
public export
code_508 : StatusCode
code_508 = MkStatusCode 508

||| 510 - Not Extended
public export
code_510 : StatusCode
code_510 = MkStatusCode 510

||| 511 - Network Authentication Required
public export
code_511 : StatusCode
code_511 = MkStatusCode 511

||| Get the status message for a status code
||| @ code The status code
|||
||| Returns the status message for the status code
export
statusMessage : StatusCode -> String
statusMessage code = case code of
  (MkStatusCode 100) => "Continue"
  (MkStatusCode 101) => "Switching Protocols"
  (MkStatusCode 102) => "Processing"
  (MkStatusCode 103) => "Early Hints"
  (MkStatusCode 200) => "OK"
  (MkStatusCode 201) => "Created"
  (MkStatusCode 202) => "Accepted"
  (MkStatusCode 203) => "Non-Authoritative Information"
  (MkStatusCode 204) => "No Content"
  (MkStatusCode 205) => "Reset Content"
  (MkStatusCode 206) => "Partial Content"
  (MkStatusCode 207) => "Multi-Status"
  (MkStatusCode 208) => "Already Reported"
  (MkStatusCode 226) => "IM Used"
  (MkStatusCode 300) => "Multiple Choices"
  (MkStatusCode 301) => "Moved Permanently"
  (MkStatusCode 302) => "Found"
  (MkStatusCode 303) => "See Other"
  (MkStatusCode 304) => "Not Modified"
  (MkStatusCode 305) => "Use Proxy"
  (MkStatusCode 307) => "Temporary Redirect"
  (MkStatusCode 308) => "Permanent Redirect"
  (MkStatusCode 400) => "Bad Request"
  (MkStatusCode 401) => "Unauthorized"
  (MkStatusCode 403) => "Forbidden"
  (MkStatusCode 404) => "Not Found"
  (MkStatusCode 405) => "Method Not Allowed"
  (MkStatusCode 406) => "Not Acceptable"
  (MkStatusCode 407) => "Proxy Authentication Required"
  (MkStatusCode 408) => "Request Timeout"
  (MkStatusCode 409) => "Conflict"
  (MkStatusCode 410) => "Gone"
  (MkStatusCode 411) => "Length Required"
  (MkStatusCode 412) => "Precondition Failed"
  (MkStatusCode 413) => "Payload Too Large"
  (MkStatusCode 414) => "URI Too Long"
  (MkStatusCode 415) => "Unsupported Media Type"
  (MkStatusCode 416) => "Range Not Satisfiable"
  (MkStatusCode 417) => "Expectation Failed"
  (MkStatusCode 418) => "I'm a teapot"
  (MkStatusCode 421) => "Misdirected Request"
  (MkStatusCode 422) => "Unprocessable Entity"
  (MkStatusCode 423) => "Locked"
  (MkStatusCode 424) => "Failed Dependency"
  (MkStatusCode 425) => "Too Early"
  (MkStatusCode 426) => "Upgrade Required"
  (MkStatusCode 428) => "Precondition Required"
  (MkStatusCode 429) => "Too Many Requests"
  (MkStatusCode 431) => "Request Header Fields Too Large"
  (MkStatusCode 451) => "Unavailable For Legal Reasons"
  (MkStatusCode 500) => "Internal Server Error"
  (MkStatusCode 501) => "Not Implemented"
  (MkStatusCode 502) => "Bad Gateway"
  (MkStatusCode 503) => "Service Unavailable"
  (MkStatusCode 504) => "Gateway Timeout"
  (MkStatusCode 505) => "HTTP Version Not Supported"
  (MkStatusCode 506) => "Variant Also Negotiates"
  (MkStatusCode 507) => "Insufficient Storage"
  (MkStatusCode 508) => "Loop Detected"
  (MkStatusCode 510) => "Not Extended"
  (MkStatusCode 511) => "Network Authentication Required"
  (MkStatusCode code) => "Unknown Status Code"