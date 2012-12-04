definition module SharedFile

import FilePath, SharedDataSource

sharedFile :: !FilePath !(String -> a) !(a -> String) -> Shared a *World