data A = A | B
  deriving (Eq, Show)
sigma 0  = A
sigma n
  | sum 0 0 (n-1) <= sum 1 0 (n-1) = A
  | otherwise = B
  where
    sum 0 acc i
      | i == 0       = acc + 1
      | sigma i == B = acc
      | otherwise    = sum 0 (acc+1) (i-1)
    sum n acc i
      | i == 0       = acc
      | sigma i == B = sum (n-1) acc (i-1)
      | otherwise    = sum  n    acc (i-1)
