data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr
    | Literal Float



eval :: Expr -> Float
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Literal f1) = f1

-- Should output "5.0"
test1 = 
    let input = Plus (Literal 3.0) (Literal 2.0) in
    eval input

-- Should output "3.5"
test2 = 
    let input = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0)) in
    eval input

-- Should output "15.5"
test3 = 
    let input = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0)) in
    eval input

equals :: Expr -> Expr -> Bool
equals (Plus e1 e2) (Plus e3 e4) = equals e1 e3 && equals e2 e4
equals (Minus e1 e2) (Minus e3 e4) = equals e1 e3 && equals e2 e4
equals (Times e1 e2) (Times e3 e4) = equals e1 e3 && equals e2 e4
equals (Div e1 e2) (Div e3 e4) = equals e1 e3 && equals e2 e4
equals (Literal x) (Literal y) = x == y
equals _ _ = False

-- Should output "True"
test4 =
    let input1 = Plus (Literal 5.0) (Div (Literal 1.0) (Literal 2.0)) in
    equals input1 input1

-- Should output "True"
test5 =
    let input1 = Plus (Literal 5.0) (Div (Literal 1.0) (Literal 2.0)) in
    let input2 = Plus (Literal 5.0) (Div (Literal 1.0) (Literal 2.0)) in
    equals input1 input2

-- Should output "False"
test6 =
    let input1 = Plus (Literal 5.0) (Div (Literal 1.0) (Literal 2.0)) in
    let input2 = Plus (Literal 5.0) (Literal 2.0) in
    equals input1 input2
