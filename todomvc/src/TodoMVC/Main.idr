||| Define example API routes and handler functions
module TodoMVC.Main

import Pact.Server
import JSON.ToJSON

import JSON.Derive

%language ElabReflection

record TodoId where
  constructor MkTodoId
  id : Nat 

implementation HasPathParam TodoId where
  parsePathParams s = case parsePositive s of
    Just n => Just (MkTodoId n)
    Nothing => Nothing

implementation Interpolation TodoId where
  interpolate (MkTodoId n) = "\{show n}"

%runElab derive "TodoId" [Show,Eq,ToJSON, FromJSON]

record Todo where
  constructor MkTodo
  id : TodoId
  title : String
  completed : Bool

%runElab derive "Todo" [Show,Eq,ToJSON, FromJSON]


todos : List Todo
todos = [ 
  MkTodo (MkTodoId 1) "Todo 1" False
  , MkTodo (MkTodoId 2) "Todo 2" True
  , MkTodo (MkTodoId 3) "Todo 3" False
  , MkTodo (MkTodoId 4) "Todo 4" True
  , MkTodo (MkTodoId 5) "Todo 5" False
  , MkTodo (MkTodoId 6) "Todo 6" True
  , MkTodo (MkTodoId 7) "Todo 7" False
  , MkTodo (MkTodoId 8) "Todo 8" True
  , MkTodo (MkTodoId 9) "Todo 9" False
  , MkTodo (MkTodoId 10) "Todo 10" True
  , MkTodo (MkTodoId 11) "Todo 11" False
  , MkTodo (MkTodoId 12) "Todo 12" True
  , MkTodo (MkTodoId 13) "Todo 13" False
  ]


ApiGetTodos = StaticPath "todos" :> Get JSONAccept (List Todo)

handlerGetTodos : GetHandlerType IO ApiGetTodos
handlerGetTodos = pure todos

routeGetTodos : RouteItem IO
routeGetTodos = ApiGetTodos :=> handlerGetTodos

ApiGetTodo = StaticPath "todos" :/ Capture "id" TodoId :> Get JSONAccept Todo

handlerGetTodo : GetHandlerType IO ApiGetTodo
handlerGetTodo id = pure $ MkTodo id "Todo \{id}" False

routeGetTodo : RouteItem IO
routeGetTodo = ApiGetTodo :=> handlerGetTodo


router : Router IO
router = MkRouter [ routeGetTodos, routeGetTodo ]


||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  -- Create server config, set max connections to 1
  let config = { maxConns := 1 , bind :=  IP4 [0,0,0,0] 2222 } defaultConfig
  putStrLn "Starting server on the http://localhost:\{show config.bind.port}"
  -- Run the server with the config and application
  runRouter config router
