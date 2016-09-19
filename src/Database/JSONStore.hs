{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Database.JSONStore where

import System.IO
import Control.Monad(forM)
import Data.Maybe(catMaybes)


import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import System.Directory

import Database.JSONStore.Manifest
import Database.JSONStore.Key
import Database.JSONStore.Settings

put :: ToJSON a => Settings -> JSONKey -> a -> IO ()
put s k d = do
    let k' = init k
        l = last k
    makeManifests s k
    makeFile s k "doc.json" (\f h -> do
        LB.hPut h (encode d)
        updateManifest s k l (singleDocument f)
        )

putAttachment :: Settings -> JSONKey -> AttachmentClass -> AttachmentKey -> LB.ByteString -> IO ()
putAttachment s k c ak content = do
    let k' = init k
        l = last k
    makeManifests s k
    makeFile s k ak (\f h -> do
        LB.hPut h content
        updateManifest s k l (singleAttachment c ak f)
        )

get :: FromJSON a => Settings -> JSONKey -> IO (Maybe a)
get s k = do 
    (man, _) <- loadManifest s k
    case lastDocument s k man of
        Nothing -> return Nothing
        Just doc -> do
            json <- LB.readFile doc
            return (decode json)


getSubKeys :: Settings -> JSONKey -> IO [JSONKey]
getSubKeys s k = do
    (man, _) <- loadManifest s k
    return $ do
        sk <- allKeys man
        return (k ++ [sk])

getRevisions :: FromJSON a => Settings -> JSONKey -> IO [a]
getRevisions s k = do
    (man, _) <- loadManifest s k
    res <- forM (allDocuments s k man) $ \doc -> do
        exist <- doesFileExist doc
        if exist
            then do
                json <- LB.readFile doc
                return (decode json)
            else return Nothing
    return (catMaybes res)

getAttachments :: Settings -> JSONKey -> IO [(AttachmentClass, AttachRef)]
getAttachments = allAttachments

getAttachmentData :: Settings -> AttachRef -> IO (Maybe LB.ByteString)
getAttachmentData s ar = attachRefFile s ar >>= \case
    Nothing -> return Nothing
    Just path -> LB.readFile path >>= return . Just



getAttachmentRevisions :: Settings -> AttachRef -> IO [LB.ByteString]
getAttachmentRevisions s ar = do
    revs <- attachmentRevisions s ar
    forM revs LB.readFile


