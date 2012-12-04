definition module ostick

// to be placed in something bigger later

::	Tick

pack_tick	::	!Int -> Tick
unpack_tick	::	!Tick -> Int

os_getcurrenttick :: !*World -> (!Tick, !*World)
