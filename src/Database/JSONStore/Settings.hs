module Database.JSONStore.Settings where

import qualified System.FilePath as FS

data Settings = Settings
    { rootDir :: FS.FilePath
    , reconcile :: Int
    } deriving (Show)
