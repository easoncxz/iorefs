module EuclideanDivision where

divModEuclidean :: Integral a => a -> a -> (a, a)
divModEuclidean a b =
  if b < 0
    then let (q, r) = divModEuclidean a (-b)
          in (-q, r)
    else if a < 0
           then let (q, r) = divModEuclidean (-a) b
                 in (-q - 1, -r + b)
           else quotRem a b
