module EuclideanDivision where

divModEuclidean :: Integral a => a -> a -> (a, a)
divModEuclidean a b =
  if b < 0
    then let (q, r) = divModEuclidean a (-b)
          in (-q, r)
    else if a < 0
           then let (q, r) = divModEuclidean (-a) b
                 in if r == 0
                      then (-q, r)
                      else (-q - 1, -r + b)
           else quotRem a b

divModEuclidean' :: Integral a => a -> a -> (a, a)
divModEuclidean' a b =
  if b < 0
    then let (q, r) = divModEuclidean' a (-b)
          in (-q, r)
    else divMod a b

divModByQuotRem :: Integral a => a -> a -> (a, a)
divModByQuotRem a b =
  let (q, r) = quotRem a b
   in if r /= 0 && (a < 0 && b > 0 || a > 0 && b < 0)
        then (q - 1, r + b)
        else (q, r)

quotRemByDivMod :: Integral a => a -> a -> (a, a)
quotRemByDivMod a b =
  let (q, r) = divMod a b
   in if r /= 0 && (a < 0 && b > 0 || a > 0 && b < 0)
        then (q + 1, r - b)
        else (q, r)
