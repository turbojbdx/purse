module Main where

import Entry
import User

main :: IO ()
main = do 
  user <- setupUser
  printUser user