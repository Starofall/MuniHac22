module Main(main) where

import LovecraftLetter.Core
  
main :: IO ()
main = mapM_ print deck
  
