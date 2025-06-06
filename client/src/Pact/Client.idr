module Pact.Client

import Network.HTTP
import Utils.String
import public Pact.Client.Core
import public Network.HTTP.URL
import Data.Either

import Control.Monad.Either

test : String
test = "Hello from Idris2!"


map_error : Functor m => (e -> e') -> EitherT e m a -> EitherT e' m a
map_error f = bimapEitherT f id


with_client : {e : _} -> IO (HttpClient e) -> (HttpClient e -> EitherT (HttpError e) IO a) -> EitherT (HttpError e) IO a
with_client client f = MkEitherT $ do
  c <- client
  Right ok <- runEitherT (f c)
  | Left err => close c *> pure (Left err)
  close c
  pure (Right ok)

url2 : String
-- url = "https://httpbin.org/html?t=哈哈"
url2 = "https://www.mugeda.com/"

export
get : (url : URL) -> EitherT String IO String
get url = map_error show $ with_client {e=()} new_client_default $ \client => do
  (response, content) <- request client GET url [] ()
  content <- toList_ content
  case utf8_pack content of
    Nothing => pure "Failed to decode content"
    Just content => pure content

main: IO ()
main = do
  Right _ <- runEitherT $ get (url' url2)
  | Left err => printLn err
  pure ()