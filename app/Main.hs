{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fplugin GHC.Syntax.QualifiedDo.Plugin #-}

module Main where

import IxSyntax (IxMonad, ipure)
import qualified IxSyntax as Ix
import qualified MonoidSyntax as Mon

theStr :: String
theStr =
  Mon . do
    "foo"
    "bar"
    x <- "duu"
    x ++ "duz"
    "quux"

imwhen :: IxMonad m => m i j Bool -> m j k a -> m j k () -> m i k ()
imwhen mbool act fallback =
  Ix . do
    p <- mbool
    if p
      then Ix .do { act; ipure () }
      else fallback

main :: IO ()
main = putStrLn theStr
