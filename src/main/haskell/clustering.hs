neighbors :: Int -> Int -> Int -> Int -> [Int]
neighbors x 0 _ _ = [x]
neighbors x dist start length =