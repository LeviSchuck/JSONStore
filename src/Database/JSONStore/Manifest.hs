{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.JSONStore.Manifest where

import Data.Monoid((<>))
import Control.Monad(forM,when,unless)
import Data.Maybe(catMaybes)
import Data.Char(ord)
import System.IO(openBinaryTempFile,hClose,Handle(..))
import Control.Exception(bracket)

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LB
import qualified System.FilePath as FS
import System.FilePath((<.>),(</>))
import System.Directory


import Database.JSONStore.Key
import Database.JSONStore.Settings

type FileName = T.Text
type FileType = T.Text

data Manifest = Manifest
    { manifestDocuments :: V.Vector FileName
    , manifestChildren :: S.Set Key
    , manifestAttachments :: M.Map AttachmentClass (M.Map AttachmentKey (V.Vector FileName))
    } deriving (Show)

instance Monoid Manifest where
    mempty = Manifest
        { manifestDocuments = V.empty
        , manifestChildren = S.empty
        , manifestAttachments = M.empty
        }
    mappend m1 m2 = Manifest
        { manifestDocuments = (manifestDocuments m1) <> (manifestDocuments m2)
        , manifestChildren = (manifestChildren m1) <> (manifestChildren m2)
        , manifestAttachments = M.unionWith (M.unionWith (<>)) (manifestAttachments m1) (manifestAttachments m2)
        }

instance ToJSON Manifest where
    toJSON Manifest{..} = object (catMaybes
        [ if (not . V.null) manifestDocuments
            then Just ("docs" .= manifestDocuments)
            else Nothing
        , if (not . S.null) manifestChildren
            then Just ("children" .= manifestChildren)
            else Nothing
        , if M.foldl (M.foldl (\a v -> a || not (V.null v))) False manifestAttachments
            then Just ("attachments" .= toJSON1 manifestAttachments)
            else Nothing
        ])

instance FromJSON Manifest where
    parseJSON (Object v) = Manifest
        <$> v .:? "docs" .!= mempty
        <*> v .:? "children" .!= mempty
        <*> v .:? "attachments" .!= mempty 
    parseJSON _ = mempty

manifestExtension = ".manifest"

documentBase = "doc"

makePath :: Settings -> JSONKey -> FS.FilePath
makePath s key = (rootDir s) </> FS.joinPath (map T.unpack key)

loadManifest :: Settings -> JSONKey -> IO (Manifest, Int)
loadManifest s key = do
    let path = makePath s key </> ("" <.> manifestExtension)
    exists <- doesFileExist path
    file <- if exists
        then LB.readFile path
        else return LB.empty
    let manifestRevisions = LB.split (fromIntegral $ ord '\n') file
        manifestsM = map decode manifestRevisions
        manifests = catMaybes manifestsM
        single = foldl (<>) mempty manifests
        count = if exists then length manifests else 0
    return (single, count)

singleDocument :: T.Text -> Manifest
singleDocument f = mempty {manifestDocuments = V.singleton f}

lastDocument :: Settings -> JSONKey -> Manifest -> Maybe FS.FilePath
lastDocument s k Manifest{..} = if V.null manifestDocuments
    then Nothing
    else let p = makePath s k
             l = T.unpack (V.last manifestDocuments)
         in Just (p </> l)

allDocuments :: Settings -> JSONKey -> Manifest -> [FS.FilePath]
allDocuments s k Manifest{..} = do
    let p = makePath s k
    doc <- V.toList manifestDocuments
    let dp = T.unpack doc
    return (p </> dp)


allAttachments :: Settings -> JSONKey -> IO [(AttachmentClass, AttachRef)]
allAttachments s k = do
    (man,_) <- loadManifest s k
    return $ do
        (cl,ks) <- M.toList (manifestAttachments man)
        ak <- M.keys ks
        let lastRev = case M.lookup ak ks of
                Nothing -> 0
                Just revs -> V.length revs - 1
        return (cl, (k, cl, ak, lastRev))

attachRefFile :: Settings -> AttachRef ->IO (Maybe FS.FilePath)
attachRefFile s (k, cl, ak, rev) = do
    (man, _) <- loadManifest s k
    case M.lookup cl (manifestAttachments man) of
        Nothing -> return Nothing
        Just ks -> case M.lookup ak ks of
            Nothing -> return Nothing
            Just revs -> case revs V.!? rev of
                Nothing -> return Nothing
                Just revfile -> do
                    let path = makePath s k </> (T.unpack revfile)
                    exists <- doesFileExist path
                    if exists
                        then return (Just path)
                        else return Nothing


singleAttachment :: AttachmentClass -> AttachmentKey -> T.Text -> Manifest
singleAttachment c k f = mempty {manifestAttachments = M.singleton c (M.singleton k (V.singleton f))}

allKeys :: Manifest -> [Key]
allKeys Manifest{..} = S.toList manifestChildren

updateManifest :: Settings -> JSONKey -> Key -> Manifest -> IO ()
updateManifest s k l nm = do
    let path = makePath s k
        parent = path </> ("" <.> manifestExtension)
    (man, count) <- loadManifest s k
    unless (S.member l (manifestChildren man)) $ do
        if count > 0
            then if count > reconcile s
                then LB.writeFile parent (encode (man <> nm))
                else LB.appendFile parent (LB.append "\n" (encode nm))
            else LB.writeFile parent (encode nm)

makeManifests :: Settings -> JSONKey -> IO ()
makeManifests _ [] = return ()
makeManifests s k = do
    let k' = init k
        l = last k
        parent = makePath s k'
        child = parent </> T.unpack l
        nm = mempty {manifestChildren = S.singleton l}
    -- Make parents as necessary
    makeManifests s k'
    updateManifest s k' l nm
    createDirectoryIfMissing True child

makeFile :: Settings -> JSONKey -> T.Text -> (T.Text -> Handle -> IO ()) -> IO ()
makeFile s k n f = bracket
    (openBinaryTempFile (makePath s k) (T.unpack n))
    (\(_,h) -> hClose h)
    (\(p,h) -> f (T.pack (FS.takeFileName p)) h)

