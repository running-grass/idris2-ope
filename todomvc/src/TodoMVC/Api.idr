module TodoMVC.Api

import TodoMVC.Data
import Pact.API
import Data.Vect
import Data.Vect.Quantifiers

public export
ApiGetTodos : API
ApiGetTodos = StaticPath "todos" :> Get JSONAccept (List Todo)

public export
ApiGetTodo : API
ApiGetTodo = StaticPath "todos" :/ Capture "id" TodoId :> Get JSONAccept Todo


public export
ApiPostTodo : API
ApiPostTodo = StaticPath "todos" :/ ReqBody Todo :> Post JSONAccept Todo


testFn : (api: API) -> {auto allprf : All ToHttpApiData api.types} -> {auto verbPrf: MimeUnrender (VerbAccept api.verb) (VerbResponse api.verb)} -> String
testFn api = "test"