||| HTTP versions
module Pact.WAI.Version

import Derive.Prelude
import Pact.WAI.HTTPErr

%language ElabReflection


||| HTTP version enum
||| Defines supported HTTP protocol versions
public export
data Version = V10 | V11 | V20

%runElab derive "Version" [Show,Eq,Ord]


||| Parse HTTP version string
||| 
||| Convert string to Version enum value
||| @ s Version string (e.g. "HTTP/1.0", "HTTP/1.1", etc.)
public export
version : String -> Either HTTPErr Version
version "HTTP/1.0" = Right V10
version "HTTP/1.1" = Right V11
version "HTTP/2.0" = Right V20
version _          = Left InvalidRequest

