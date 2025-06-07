module Pact.Client

import Network.HTTP
import Utils.String
import public Pact.Client.Core
import public Network.HTTP.URL
import Data.Either

import Control.Monad.Either

test : String
test = "Hello from Idris2!"

url2 : String
url2 = "https://www.mugeda.com/"

main: IO ()
main = do
  Right _ <- runEitherT $ get (url' url2)
  | Left err => printLn err
  pure ()