module Pact.API.HasPathParam

import Data.String
import Data.Maybe
import Data.Nat
import Data.Double

public export
interface HasPathParam a where
  parsePathParams : String -> Maybe a

public export
implementation HasPathParam () where
  parsePathParams _ = Just ()

public export
implementation HasPathParam String where
  parsePathParams s = Just s

public export
implementation HasPathParam Nat where
  parsePathParams = parsePositive

public export
implementation HasPathParam Int where
  parsePathParams = parseInteger

public export
implementation HasPathParam Integer where
  parsePathParams = parseInteger

public export
implementation HasPathParam Double where
  parsePathParams = parseDouble

public export
implementation HasPathParam Bool where
  parsePathParams "0" = Just False
  parsePathParams "1" = Just True
  parsePathParams _ = Nothing
