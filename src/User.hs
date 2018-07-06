{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module User(User (..), setupUser) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified System.IO as IO
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Aeson
import Crypto.BCrypt
import System.Directory
import System.FilePath
import GHC.Generics

data User = User { masterPw :: T.Text } deriving(Generic, Show)

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "masterPw"

userFile :: IO FilePath
userFile = do return "pursefile.json"

userFolder :: IO FilePath
userFolder = do return ".purse"

setUserFolder :: IO ()
setUserFolder = do 
                home   <- getHomeDirectory
                folder <- userFolder
                createDirectoryIfMissing (False) (joinPath [home, folder])

writeFileIfItDoesntExist :: FilePath -> FilePath -> FilePath -> IO () 
writeFileIfItDoesntExist home folder file = do 
                                            exists <- doesFileExist(joinPath [home, folder, file])
                                            if   exists
                                            then return ()
                                            else Prelude.writeFile (joinPath [home, folder, file]) "{}"

setUserFile :: IO ()
setUserFile = do 
              home   <- getHomeDirectory
              folder <- userFolder
              file   <- userFile
              writeFileIfItDoesntExist home folder file

extractUser :: (Maybe User) -> IO User
extractUser user = case user of 
                        Just u  -> return u
                        Nothing -> do  
                                   print "No master pw saved. Input a master pw: "
                                   pw <- IO.readLn :: IO String
                                   return (User { masterPw = "" })

 -- return (User { masterPw = "" })
-- extractUser (Just user) = return (user)
-- setMasterPW :: IO User -> User


setupUserMasterPW :: IO User -> IO User
setupUserMasterPW user = user

loadUser :: IO User
loadUser = do 
           home       <- getHomeDirectory
           folder     <- userFolder
           file       <- userFile
           str        <- Prelude.readFile(joinPath [home, folder, file])
           userWithPw <- setupUserMasterPW (extractUser ((decode $ C8.pack(str)) :: Maybe User))
           return $ userWithPw

printUser :: User -> IO ()
printUser user = Prelude.putStrLn $ show user

setupUser :: IO ()
setupUser = setUserFolder >> 
            setUserFile   >>
            loadUser      >>=
            printUser