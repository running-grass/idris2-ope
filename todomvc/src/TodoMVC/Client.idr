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
  let url' = "http://localhost:2222" ++ (generateLinkByAPI ApiGetTodo $ MkTodoId 2)
  case url_from_string url' of
    Left err => printLn err
    Right url => do
      liftIO $ printLn url
      Right content <- runEitherT $ get url
      | Left err => printLn err
      printLn content
      pure ()
