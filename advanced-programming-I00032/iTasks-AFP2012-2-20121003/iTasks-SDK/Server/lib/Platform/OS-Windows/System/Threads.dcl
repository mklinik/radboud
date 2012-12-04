definition module Threads

import StdDynamic

:: ThreadId :== DWORD

getCurrentThreadId :: !*World -> (!ThreadId, !*World)
send :: !ThreadId !a !*World -> *(!Bool, !*World) | TC a
receive :: !*World -> *(!Int, !Dynamic, !*World)

fork :: !(*World -> *World) !*World -> (!ThreadId, !*World)
waitForThread :: !ThreadId !*World -> *World

from _WinDef import :: LPVOID, :: DWORD

threadFunc :: !LPVOID -> DWORD