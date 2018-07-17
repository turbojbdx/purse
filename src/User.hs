{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module User(User (..), setupUser, printUser) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified System.IO as IO
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Crypto.BCrypt as BCrypt

import Data.Aeson
import Data.Maybe
import Data.Strings
import Data.Char (chr)
import System.Directory
import System.FilePath
import GHC.Generics

data User = 
    User    { masterPw :: T.Text } |
    UserNew { masterPw :: T.Text } |
    UserInvalid 
    deriving(Generic, Show)

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "masterPw"

instance ToJSON User where
    -- this generates a Value
    toJSON (User masterPw)    = object ["masterPw" .= masterPw]
    toJSON (UserNew masterPw) = object ["masterPw" .= masterPw]

userFile :: IO FilePath 
userFile = do return "pursefile.json"

userFolder :: IO FilePath
userFolder = do return ".purse"

setUserFolder :: IO ()
setUserFolder = do 
  home   <- getHomeDirectory
  folder <- userFolder
  createDirectoryIfMissing (False) (joinPath [home, folder])

writeFileIfItDoesntExist :: FilePath -> FilePath -> FilePath -> String -> IO () 
writeFileIfItDoesntExist home folder file contents = do 
  exists <- doesFileExist(joinPath [home, folder, file])
  if   exists
  then return ()
  else Prelude.writeFile (joinPath [home, folder, file]) contents

setUserFile :: IO ()
setUserFile = do 
  home   <- getHomeDirectory
  folder <- userFolder
  file   <- userFile
  writeFileIfItDoesntExist home folder file "{}"

encryptMasterPw pw = BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (BS.pack(pw))

extractUser :: (Maybe User) -> IO User
extractUser (Just u) = return u
extractUser Nothing  = do  
  putStr "No master pw saved.\nInput a new master pw: "
  -- see https://stackoverflow.com/questions/2500459/wrong-io-actions-order-using-putstr-and-getline
  IO.hFlush IO.stdout
  pw <- IO.getLine
  putStrLn "Encrypting master password ..."
  encryptedPw <- encryptMasterPw (map charToByte pw)
  let encryptedPwStr = fromJust encryptedPw
  -- this converts BS.ByteString to String. its painful
  let str            = map (chr . fromEnum) . BS.unpack $ encryptedPwStr
  let user           = UserNew { masterPw = T.pack(str) }
  let userJSON       = C8.unpack(encode user)
  home   <- getHomeDirectory
  folder <- userFolder
  file   <- userFile
  Prelude.writeFile (joinPath [home, folder, file]) userJSON
  return user

loadUser :: IO User
loadUser = do 
  home       <- getHomeDirectory
  folder     <- userFolder
  file       <- userFile
  str        <- Prelude.readFile(joinPath [home, folder, file])
  userWithPw <- extractUser ((decode $ C8.pack(str)) :: Maybe User)
  return $ userWithPw

printUser :: User -> IO ()
printUser user = Prelude.putStrLn $ show user

validateUser :: IO User
validateUser user = do
   putStr "Please input the masterpassword: "
   -- see https://stackoverflow.com/questions/2500459/wrong-io-actions-order-using-putstr-and-getline
   IO.hFlush IO.stdout
   pw <- IO.getLine
   

setupUser :: IO User
setupUser = do
  setUserFolder 
  setUserFile
  --sreturn User { masterPw = "blah" }
  user <- loadUser
  return user