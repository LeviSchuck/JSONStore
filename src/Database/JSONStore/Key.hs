module Database.JSONStore.Key where

import qualified Data.Text as T
import qualified System.FilePath as FS

type Key = T.Text
type JSONKey = [Key]
type AttachmentKey = Key
type AttachmentClass = T.Text
type AttachRef = (JSONKey, AttachmentClass, AttachmentKey, Int)
