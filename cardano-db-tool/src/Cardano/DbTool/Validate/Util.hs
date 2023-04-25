module Cardano.DbTool.Validate.Util (
  codeGreen,
  codeRed,
  codeReset,
  fmap2,
  greenText,
  redText,
  putStrF,
) where

import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types (
  Color (..),
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
 )
import System.IO (hFlush, stdout)

codeGreen :: String
codeGreen = setSGRCode [SetColor Foreground Vivid Green]

codeRed :: String
codeRed = setSGRCode [SetColor Foreground Vivid Red]

codeReset :: String
codeReset = setSGRCode [Reset]

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

greenText :: String -> String
greenText s = codeGreen ++ s ++ codeReset

redText :: String -> String
redText s = codeRed ++ s ++ codeReset

putStrF :: String -> IO ()
putStrF s = putStr s >> hFlush stdout
