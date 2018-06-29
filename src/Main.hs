{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Web.Twitter.Conduit
import Web.Twitter.Conduit.Base
import Web.Twitter.Types.Lens

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary (sinkFile)

import Data.Default
import Data.List
import Data.Maybe (catMaybes)

import Network.HTTP.Conduit as HTTP
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA

import System.IO (hFlush, stdout)
import System.Environment
import System.Directory

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = ""
    , oauthConsumerSecret = ""
    }

credential :: Credential
credential = Credential
    [ ("oauth_token", "")
    , ("oauth_token_secret", "")
    ]

twInfo :: TWInfo
twInfo = def
    { twToken = def { twOAuth = tokens, twCredential = credential }
    , twProxy = Nothing
    }

downloadImage :: HTTP.Manager -> String -> String -> IO ()
downloadImage mgr accountName imageURL = do
    createDirectoryIfMissing False $ "./images/" ++ accountName
    let filename' = stripPrefix "http://pbs.twimg.com/media/" imageURL
    case filename' of
        Nothing -> return ()
        Just filename -> do
            fileCheck <- doesFileExist $ "./images/" ++ accountName ++ "/" ++ filename
            if fileCheck 
                then return ()
                else do
                    request <- parseUrl $ imageURL ++ ":orig"
                    runResourceT $ do
                        response <- http request mgr
                        HTTP.responseBody response C.$$+- sinkFile
                                                    $ "./images/" ++ accountName ++ "/" ++ filename

filterURL :: [Status] -> [String]
filterURL = fmap T.unpack . fmap (^. entityBody . meMediaURL)
                . concat . fmap (^. enMedia) . catMaybes . fmap (^. statusExtendedEntities)

getImages :: String -> Integer -> IO ()
getImages screenName tweets = HTTP.withManager $ \mgr -> do
    res <- call twInfo mgr $ userTimeline (ScreenNameParam screenName)
                                  & includeRts ?~ False & count ?~ tweets
    mapM_ (liftIO . downloadImage mgr screenName) . filterURL $ res

main :: IO ()
main = do
    args <- getArgs
    if length args == 2
        then do
            let screenName = args !! 0
                tweets = read (args !! 1) :: Integer
            getImages screenName tweets
        else return ()