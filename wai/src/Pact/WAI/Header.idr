||| HTTP header type
module Pact.WAI.Header

import Data.SortedMap

||| HTTP header type
||| Represented as a key-value map, both key and value are strings
public export
0 Headers : Type
Headers = SortedMap String String


public export
emptyHeaders : Headers
emptyHeaders = empty