definition module StdDynamicFileIO

from StdFile import class FileSystem
// FIXME: remove this when the compiler no longer translates
// :: Dynamic to :: DynamicTemp
from _SystemDynamic import :: DynamicTemp

readDynamic :: String *f -> (Bool,Dynamic,*f) | FileSystem f

writeDynamic :: String Dynamic *f -> (Bool,*f) | FileSystem f
