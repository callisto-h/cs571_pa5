
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (h1:t1) (h2:t2)
    | h1 == h2 = prefix t1 t2
    | otherwise = False

-- Should output "True"
test1 = 
    let input1 = [1,2,3] in
    let input2 = [1,2,3,4] in
    prefix input1 input2

-- Should output "False"
test2 = 
    let input1 = [1,2,3] in
    let input2 = [4,5,6] in
    prefix input1 input2



sublist :: [a] -> [[a]]
sublist []     = [[]]
sublist (h:t) = let rest = sublist t
                 in map (h:) rest ++ rest
                 
-- Should output "[[], [1], [2], [3], [1, 2], [2, 3], [1,3], [1, 2, 3]]"
test3 =
    let input = [1,2,3] in
    sublist input



