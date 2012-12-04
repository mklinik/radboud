implementation module Environment

import StdOverloaded, StdInt, StdArray, StdChar, StdString
import Maybe
import _Pointer

MAXBUF :== 32767 //Maximum size for environment variables

getEnvironmentVariable :: !String !*World -> (Maybe String, *World)
getEnvironmentVariable name world
	# buf	= createArray MAXBUF '\0'
	# len	= getenvC (packString name) buf MAXBUF
	| len == 0	= (Nothing, world)
				= (Just (buf % (0, len - 1)), world)
	where
		getenvC :: !{#Char} !{#Char} !Int -> Int
		getenvC a0 a1 a2 = code {
			ccall GetEnvironmentVariableA@12 "PssI:I"
		}

setEnvironmentVariable :: !String !String !*World -> *World
setEnvironmentVariable name value world
	# (_,world) = setenvC (packString name) (packString value) world
	= world
	where
		setenvC :: !{#Char} !{#Char} !*World -> (!Int, !*World)
		setenvC a0 a1 a2 = code {
			ccall SetEnvironmentVariableA@8 "Pss:I:A"
		}

unsetEnvironmentVariable :: !String !*World -> *World
unsetEnvironmentVariable name world
	# (_,world) = unsetenvC (packString name) 0 world
	= world
	where
		unsetenvC :: !{#Char} !Int !*World -> (!Int, !*World)
		unsetenvC a0 a1 a2 = code {
			ccall SetEnvironmentVariableA@8 "PsI:I:A"
		}