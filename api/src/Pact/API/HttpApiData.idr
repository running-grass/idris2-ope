module Pact.API.HttpApiData

import Data.String
import Data.Vect
import Data.List1

public export
interface FromHttpApiData a where
  parseUrlPiece : String -> Either String a

  parseHeader : String -> Either String a
  parseHeader = parseUrlPiece

  parseQueryParam : String -> Either String a
  parseQueryParam = parseUrlPiece

public export
implementation FromHttpApiData () where
  parseUrlPiece _ = Right ()

public export
implementation FromHttpApiData Bool where
  parseUrlPiece s = case s of
    "true" => Right True
    "false" => Right False
    _ => Left "Invalid Bool"

public export
implementation FromHttpApiData Char where
  parseUrlPiece s = case asList s of
    [c] => Right c
    _ => Left "Invalid Char"

public export
implementation FromHttpApiData String where
  parseUrlPiece = Right


public export
implementation FromHttpApiData Nat where
  parseUrlPiece s = case parsePositive s of
    Just n => Right n
    Nothing => Left "Invalid Int"

public export
implementation FromHttpApiData Int where
  parseUrlPiece s = case parseInteger s of
    Just n => Right n
    Nothing => Left "Invalid Int"

public export
implementation FromHttpApiData Integer where
  parseUrlPiece s = case parseInteger s of
    Just n => Right n
    Nothing => Left "Invalid Integer"

public export
implementation FromHttpApiData Int8 where
  parseUrlPiece s = case parseInteger { a = Int8 } s of
    Just n => if n > 127 || n < -128 then Left "Invalid Int8" else Right n
    Nothing => Left "Invalid Int8"

public export
implementation FromHttpApiData Int16 where
  parseUrlPiece s = case parseInteger { a = Int16 } s of
    Just n => if n > 32767 || n < -32768 then Left "Invalid Int16" else Right n
    Nothing => Left "Invalid Int16"

public export
implementation FromHttpApiData Int32 where
  parseUrlPiece s = case parseInteger { a = Int32 } s of
    Just n => if n > 2147483647 || n < -2147483648 then Left "Invalid Int32" else Right n
    Nothing => Left "Invalid Int32"

public export
implementation FromHttpApiData Int64 where
  parseUrlPiece s = case parseInteger { a = Int64 } s of
    Just n => if n > 9223372036854775807 || n < -9223372036854775808 then Left "Invalid Int64" else Right n
    Nothing => Left "Invalid Int64"

public export
implementation FromHttpApiData Bits8 where
  parseUrlPiece s = case parsePositive { a = Bits8 } s of
    Just n => if n > 255 then Left "Invalid Bits8" else Right n
    Nothing => Left "Invalid Bits8"

public export
implementation FromHttpApiData Bits16 where
  parseUrlPiece s = case parsePositive { a = Bits16 } s of
    Just n => if n > 65535 then Left "Invalid Bits16" else Right n
    Nothing => Left "Invalid Bits16"

public export
implementation FromHttpApiData Bits32 where
  parseUrlPiece s = case parsePositive { a = Bits32 } s of
    Just n => if n > 4294967295 then Left "Invalid Bits32" else Right n
    Nothing => Left "Invalid Bits32"

public export
implementation FromHttpApiData Bits64 where
  parseUrlPiece s = case parsePositive { a = Bits64 } s of
    Just n => if n > 18446744073709551615 then Left "Invalid Bits64" else Right n
    Nothing => Left "Invalid Bits64"

public export
implementation FromHttpApiData Double where
  parseUrlPiece s = case parseDouble s of
    Just n => Right n
    Nothing => Left "Invalid Double"

public export
implementation FromHttpApiData a => FromHttpApiData (Maybe a) where
  parseUrlPiece s = case s of
    "" => Right Nothing
    _ => map Just $ parseUrlPiece s

public export
implementation Monoid a => FromHttpApiData b => FromHttpApiData (Either a b) where
  parseUrlPiece s = case s of
    "" => Right $ Left neutral
    _ => map Right $ parseUrlPiece s


public export
implementation FromHttpApiData a => FromHttpApiData (List1 a) where
  parseUrlPiece s = case s of
    "" => Left "Invalid List1"
    _ => Data.String.split (== ',') s |> map parseUrlPiece |> sequence

public export
implementation FromHttpApiData a => FromHttpApiData (List a) where
  parseUrlPiece s = case s of
    "" => Right []
    _ => map forget $ parseUrlPiece s

public export
implementation FromHttpApiData a => FromHttpApiData (n ** Vect n a) where
  parseUrlPiece s = case s of
    "" => Right (0 ** [])
    _ => parseUrlPiece {a = List a} s |> map (\list => (length list ** fromList list))

public export
interface ToHttpApiData a where
  toUrlPiece : a -> String

  toHeader : a -> String
  toHeader = toUrlPiece

  toQueryParam : a -> String
  toQueryParam = toUrlPiece

public export
implementation ToHttpApiData String where
  toUrlPiece = id

public export 
implementation ToHttpApiData () where
  toUrlPiece () = ""

public export
implementation ToHttpApiData Bool where
  toUrlPiece b = if b then "true" else "false"

public export
implementation ToHttpApiData Char where
  toUrlPiece c = singleton c

public export
implementation ToHttpApiData Nat where
  toUrlPiece = show

public export
implementation ToHttpApiData Int where
  toUrlPiece = show

public export
implementation ToHttpApiData Integer where
  toUrlPiece = show

public export
implementation ToHttpApiData Int8 where
  toUrlPiece = show

public export
implementation ToHttpApiData Int16 where
  toUrlPiece = show

public export
implementation ToHttpApiData Int32 where
  toUrlPiece = show

public export
implementation ToHttpApiData Int64 where
  toUrlPiece = show

public export
implementation ToHttpApiData Bits8 where
  toUrlPiece = show

public export
implementation ToHttpApiData Bits16 where
  toUrlPiece = show

public export
implementation ToHttpApiData Bits32 where
  toUrlPiece = show

public export
implementation ToHttpApiData Bits64 where
  toUrlPiece = show

public export
implementation ToHttpApiData Double where
  toUrlPiece = show

public export
implementation ToHttpApiData a => ToHttpApiData (Maybe a) where
  toUrlPiece = maybe "" toUrlPiece

public export
implementation ToHttpApiData a => ToHttpApiData (Either a b) where
  toUrlPiece = either toUrlPiece $ const ""

public export
implementation ToHttpApiData a => ToHttpApiData (List a) where
  toUrlPiece = joinBy "," . map toUrlPiece

public export
implementation ToHttpApiData a => ToHttpApiData (List1 a) where
  toUrlPiece = joinBy "," . toList . map toUrlPiece

public export
implementation ToHttpApiData a => ToHttpApiData (n ** Vect n a) where
  toUrlPiece (n ** v) = joinBy "," . toList $ map toUrlPiece v
