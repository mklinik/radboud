definition module Directory

from Void import :: Void
from FilePath import :: FilePath
from Error import :: MaybeError
from OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

createDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)

removeDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)

readDirectory :: !FilePath !*World -> (!MaybeOSError [FilePath], !*World)

getCurrentDirectory :: !*World -> (!MaybeOSError FilePath, !*World)

setCurrentDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
