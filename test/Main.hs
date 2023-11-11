module Main where

import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (unpack)
import Database.MongoDB.Admin (serverVersion)
import Database.MongoDB.Connection (connect, host)
import Database.MongoDB.Query (access, slaveOk)
import qualified Spec
import System.Environment (getEnv, lookupEnv)
import System.IO.Error (catchIOError)
import Test.Hspec.Runner
import TestImport

main :: IO ()
main = do
    mongodbHost <- getEnv mongodbHostEnvVariable `catchIOError` (\_ -> return "localhost")
    mongo_atlas <- lookupEnv "connection_string"
    unless (isJust mongo_atlas) $ do
        p <- connect $ host mongodbHost
        version <- access p slaveOk "admin" serverVersion
        putStrLn $ "Running tests with mongodb version: " ++ (unpack version)
        hspecWith defaultConfig Spec.spec
