implementation module StdMaybe

import Maybe, Functor

mapMaybe :: .(.x -> .y) !(Maybe .x) -> Maybe .y
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)