{-# LANGUAGE DeriveGeneric #-}

module Entry(Entry) where

import Crypto.BCrypt

data Entry = Entry { name :: String }