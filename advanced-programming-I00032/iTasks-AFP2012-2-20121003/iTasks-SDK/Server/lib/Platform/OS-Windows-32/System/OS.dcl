definition module OS

OS_NAME :== "Windows (32-bit)"
OS_PATH_SEPARATOR :== '\\'
OS_NEWLINE :== "\r\n"

IF_POSIX_OR_WINDOWS posix windows   :== windows
