WriterT (Identity (
	(),
	Seq (Assign "arg" (Const (I 10))) (
		Seq (Assign "scratch" (Var "arg")) (
			Seq (Assign "total" (Const (I 1))) (
				Seq (While (Gt (Var "scratch") (Const (I 1))) (
					Seq (Assign "total" (Mul (Var "total") (Var "scratch"))) (
						Seq (Assign "scratch" (Sub (Var "scratch") (Const (I 1)))) 
						(Print (Var "scratch")))))
				(Print (Var "total")))
			)
		)
	)
)