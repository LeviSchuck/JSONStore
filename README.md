# JSON Store

This is really only meant for storing mostly write-once data,
and is not indexed in any way.

Documents are persisted to disk with an ASCII key, where `/`
will create folders. 

It's built with `Aeson` use in mind.
