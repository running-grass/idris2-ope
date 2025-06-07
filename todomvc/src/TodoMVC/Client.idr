module TodoMVC.Client

import Pact.Client
import Control.Monad.Either
import Network.HTTP
import TodoMVC.Main
import Pact.API 
import Data.Vect

||| Program main entry function
||| Creates server configuration and starts the HTTP server
covering
main : IO ()
main = do
  let r = (generateLinkByAPI ApiGetTodo $ MkTodoId 2)
  Right content <- runEitherT r
  | Left err => printLn err
  printLn content
  