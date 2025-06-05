||| Define example API routes and handler functions
module TodoMVC.Main

import Pact.Server
import JSON.ToJSON

import JSON.Derive
import Control.Monad.Reader
import Control.Monad.Either
import Control.Monad.State
import Control.Monad.Writer

%language ElabReflection

record TodoId where
  constructor MkTodoId
  id : Nat 

implementation FromHttpApiData TodoId where
  parseUrlPiece = map MkTodoId . parseUrlPiece

implementation ToHttpApiData TodoId where
  toUrlPiece (MkTodoId n) = toUrlPiece n

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


AppM : Type -> Type
AppM = WriterT (List String) Handler

implementation Hoistable AppM where
  hoist st = do 
    let et = runWriterT st
    (a, n) <- et
    liftIO $ putStrLn "Hoisted \{show n}"
    pure a

ApiGetTodos = StaticPath "todos" :> Get JSONAccept (List Todo)

handlerGetTodos : AppM (List Todo)
handlerGetTodos = do
  tell ["Getting todos before"]
  -- do something
  tell ["Getting todos after"]
  pure todos

routeGetTodos : RouteItem AppM
routeGetTodos = ApiGetTodos :=> handlerGetTodos

ApiGetTodo = StaticPath "todos" :/ Capture "id" TodoId :> Get JSONAccept Todo

handlerGetTodo : GetHandlerType AppM ApiGetTodo
handlerGetTodo id = pure $ MkTodo id "Todo \{id}" False

routeGetTodo : RouteItem AppM
routeGetTodo = ApiGetTodo :=> handlerGetTodo

ApiPostTodo = StaticPath "todos" :/ ReqBody Todo :> Post JSONAccept Todo



getLink_ : Component _ _ _  -> SnocList String-> SnocList String
getLink_ (StaticPath path) acc = acc :< "/\{path}"
getLink_ (Capture name ty) acc = acc :< "/\{name}"
getLink_ (ReqBody _) acc = acc
getLink_ (path :/ rest) acc = getLink_ rest (acc ++ getLink_ path acc)

getLink : API ts -> String
getLink (path :> _) = foldMap id $ getLink_ path [<]

GetGenerateLinkFunType : Component _ _ _ -> Type
GetGenerateLinkFunType (StaticPath path) = String
GetGenerateLinkFunType (Capture name ty) = ty -> String
GetGenerateLinkFunType (ReqBody _) = String
GetGenerateLinkFunType (StaticPath path :/ rest) = GetGenerateLinkFunType rest
GetGenerateLinkFunType (Capture name ty :/ rest) = ty -> GetGenerateLinkFunType rest
GetGenerateLinkFunType (ReqBody _ :/ rest) = GetGenerateLinkFunType rest

GetGenerateLinkByAPI : API ts -> Type
GetGenerateLinkByAPI (path :> _) = GetGenerateLinkFunType path

generateLink : (comp: Component t ts r) -> {auto allprf : All ToHttpApiData ts} -> GetGenerateLinkFunType comp
generateLink (StaticPath path) = "/\{path}"
generateLink (Capture name ty) {allprf = prf :: restPrf} = (\x: ty => toUrlPiece x)
generateLink (ReqBody _) = ""
generateLink (StaticPath path :/ rest) {allprf = prf :: restPrf} = generateLink rest
generateLink (Capture name ty :/ rest) {allprf = prf :: restPrf} = (\x: ty => generateLink rest)
generateLink (ReqBody _ :/ _) = assert_total $ idris_crash "ReqBody is not supported"


generateLinkByAPI : (api: API ts) -> {auto allprf : All ToHttpApiData ts} -> GetGenerateLinkByAPI api
generateLinkByAPI (path :> _) = generateLink path

handlerPostTodo : Todo -> AppM Todo
handlerPostTodo todo = do
  liftIO $ putStrLn "Posting todo \{show todo}"
  pure todo

routePostTodo : RouteItem AppM
routePostTodo = ApiPostTodo :=> handlerPostTodo


router : Router AppM
router = MkRouter [ routeGetTodos, routeGetTodo, routePostTodo ]


-- app : State -> Application
-- app s = serve api $ hoistServer api (nt s) server

config : ServerConfig
config = { maxConns := 1 , bind :=  IP4 [0,0,0,0] 2222 } defaultConfig

app1 : HTTPApplication
app1 = serve router

||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  -- Create server config, set max connections to 1
  putStrLn "Starting server on the http://localhost:\{show config.bind.port}"
  run config app1
