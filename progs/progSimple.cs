WriterT (Identity ((),Seq (Assign "arg" (Const (I 10))) (Seq (If (Gt (Var "arg") (Const (I 1))) (Assign "arg" (Mul (Var "arg") (Const (I 2)))) (Assign "arg" (Mul (Var "arg") (Const (I 3))))) (Print (Var "arg")))))