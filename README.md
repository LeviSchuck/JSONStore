# JSON Store

Uses Aeson's wonderful `ToJSON` and `FromJSON` typeclasses to persist
data structures of your choosing by a hierarchical key.

This is really only meant for storing mostly write-once data,
and is not indexed in any way. Binary attachments can also be linked
to data, and like data, also have revisions.

Documents are persisted to disk and restored from Disk.

Each unique key will have its own folder on disk, keep this in mind.

## Notes

* Right now it is not too efficient though on disk
* API subject to lots of change
