definition module Section3

// Examples showing the usage of frequently used iTask combinators

import iTasks

flows3				:: [Workflow]

view			    :: !(Task a) -> Task a | iTask a

repeatUntilApproved	:: !(Task a) -> Task a | iTask a

positive			:: Task Int
