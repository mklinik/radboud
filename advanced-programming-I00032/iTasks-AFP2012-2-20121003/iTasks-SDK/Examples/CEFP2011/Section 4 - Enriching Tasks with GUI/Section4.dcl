definition module Section4

// Examples showing the extension of editors with buttons

import iTasks

flows4 :: [Workflow]

onlyIf :: (a -> Bool) (a -> Task b) (InformationState a) -> Maybe (Task b)

