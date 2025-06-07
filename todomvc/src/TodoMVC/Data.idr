module TodoMVC.Data

import JSON.Derive
import Pact.API.MimeRender
import Pact.API.HttpApiData
import Pact.API.Accept
import JSON.FromJSON
import JSON.ToJSON

%language ElabReflection

public export
record TodoId where
  constructor MkTodoId
  id : Nat 

public export
implementation FromHttpApiData TodoId where
  parseUrlPiece = map MkTodoId . parseUrlPiece

public export
implementation ToHttpApiData TodoId where
  toUrlPiece (MkTodoId n) = toUrlPiece n

public export
implementation Interpolation TodoId where
  interpolate (MkTodoId n) = "\{show n}"

%runElab derive "TodoId" [Show,Eq,ToJSON, FromJSON]

public export
record Todo where
  constructor MkTodo
  id : TodoId
  title : String
  completed : Bool

%runElab derive "Todo" [Show,Eq,ToJSON, FromJSON]

public export
implementation MimeUnrender JSONAccept Todo where
  mimeUnrender = either (Left . show) Right . decode
