||| HTTP error types
module Pact.WAI.HTTPErr

import Derive.Prelude

%language ElabReflection


||| HTTP error type
||| Represents possible errors during HTTP processing
public export
data HTTPErr : Type where
  ||| Header size exceeded
  HeaderSizeExceeded  : HTTPErr
  ||| Content size exceeded
  ContentSizeExceeded : HTTPErr
  ||| Invalid HTTP request format
  InvalidRequest      : HTTPErr

%runElab derive "HTTPErr" [Show,Eq,Ord]

||| String interpolation implementation for HTTPErr
export
Interpolation HTTPErr where
  interpolate HeaderSizeExceeded  = "header size exceeded"
  interpolate ContentSizeExceeded = "content size exceeded"
  interpolate InvalidRequest      = "invalid HTTP request"
