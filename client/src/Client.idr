module Client

import Network.HTTP
import Utils.String

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

url : String
-- url = "https://httpbin.org/html?t=哈哈"
url = "https://www.mugeda.com/"

export
test_redirect : EitherT String IO ()
test_redirect = map_error show $ with_client {e=()} new_client_default $ \client => do
  (response, content) <- request client GET (url' url) [] ()
  content <- toList_ content
  printLn . utf8_pack $ content

main: IO ()
main = do
  Right _ <- runEitherT test_redirect
  | Left err => printLn err
  pure ()