implementation module world

loadWorld :: !World -> Int
loadWorld w
	= code {
		pushI_a	0
		pop_a	1
	}

storeWorld :: !Int !World -> *World
storeWorld i w
	= code {
		fillI_b	0 1
		pop_b	1
		pop_a	1
	}
