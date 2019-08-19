
import qualified Explorer.Web (someFunc)
import Explorer.Web.Server (runServer)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Explorer.Web.someFunc
  runServer
