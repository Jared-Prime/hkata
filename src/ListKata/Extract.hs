module ListKata.Extract where

split :: [Integer] -> [[Integer]]
split xs = [(front xs), (back xs)]
  where
    half  = length xs `div` 2
    front = take half
    back  = drop half
