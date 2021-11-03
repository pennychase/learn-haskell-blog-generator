replicate' :: Int -> a -> [a]
replicate' n x 
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

even' :: Int -> Bool
even' 0 = True
even' n = odd'(n - 1)

odd' :: Int -> Bool 
odd' 0 = False 
odd' n = even' (n - 1)