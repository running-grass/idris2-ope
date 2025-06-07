module TodoMVC.Client

import Pact.Client
import Control.Monad.Either
import Network.HTTP
import TodoMVC.Data
import TodoMVC.Api
import Pact.API 
import Data.Vect

wrapper : (api: API) -> {auto allprf : All ToHttpApiData api.types} -> {auto verbPrf: MimeUnrender (VerbAccept api.verb) (VerbResponse api.verb)} -> GetGenerateLinkByAPI api
wrapper api = generateLinkByAPI api {allprf = allprf, verbPrf = verbPrf}

||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  let r = (wrapper ApiGetTodo $ MkTodoId 2)
  Right content <- runEitherT r
  | Left err => printLn err
  printLn content
  pure ()
  