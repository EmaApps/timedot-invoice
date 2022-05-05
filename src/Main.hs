module Main where

import Ema (runSite)
import Ema.Multi (MultiRoute)
import Generics.SOP
import Site.Amb (Route)
import Site.StaticSite (StaticPath)

main :: IO ()
main =
  void $ runSite @(MultiRoute '[Route, StaticPath]) (I () :* I () :* Nil)
