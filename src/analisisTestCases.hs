:l tests/ok/00-basicos/100-orden-print.fd4

let (orden_de_impresion:Nat) = print "uno! " (print "dos! " 3)

let (f:Nat->Nat) = fun (x:Nat) ->
	ifz x
	then print "uno! " 1
	else print "dos! " 2

let (test:Nat) =
	let (a : Nat) = f 0 in
	let (b : Nat) = f 1 in
	0

Let _ "f" (Arrow Nat Nat) 
    (Lam _ "x" Nat 
        {IfZ _ (Var _ (Bound 0)) 
            (Pnt _ (S {unS = "uno! "})  
                (IfZ _ (Var _ (Free "x")) 
                    (Pnt _ (S {unS = "uno! "}) (Lit _ (N {unN = 1}))) 
                    (Pnt _ (S {unS = "dos! "}) (Lit _ (N {unN = 2}))))) 
            (Pnt _ (S {unS = "dos! "}) 
                (IfZ _ (Var _ (Free "x")) 
                    (Pnt _ (S {unS = "uno! "}) (Lit _ (N {unN = 1})))
                    (Pnt _ (S {unS = "dos! "}) (Lit _ (N {unN = 2})))))
        }
    ) {Lit _ (N {unN = 0})}


Let _ "f" (Arrow Nat Nat) 
    (Lam _ "x" Nat 
        {IfZ _ (Var _ (Free "x")) 
            (Pnt _ (S {unS = "uno! "}) (Lit _ (N {unN = 1}))) 
            (Pnt _ (S {unS = "dos! "}) (Lit _ (N {unN = 2})))}
    ) 
    {Lit _ (N {unN = 0})}


let (test:Nat) =
	let (a : Nat) = f 0 in
	let (b : Nat) = f 1 in
	0

Let _ "test" Nat 
    (Let _ "a" Nat (App _   (Var _ (Global "f")) 
                            (Lit _ (N {unN = 0})))
        {Let _ "b" Nat (App _   (Var _ (Global "f")) 
                                (Lit _ (N {unN = 1}))) 
            {Lit _ (N {unN = 0})}} ) 
    {Lit _ (N {unN = 0})}