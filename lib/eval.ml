open Parser

let rec evaluate exp = 
        match exp with
        | Binary (Add, exp1, exp2) -> (evaluate exp1) +. (evaluate exp2)
        | Binary (Sub, exp1, exp2) -> (evaluate exp1) -. (evaluate exp2)
        | Binary (Mult, exp1, exp2) -> (evaluate exp1) *. (evaluate exp2)
        | Binary (Div, exp1, exp2) -> (evaluate exp1) /. (evaluate exp2)
        | Binary (Pow, exp1, exp2) -> Float.pow (evaluate exp1) (evaluate exp2)
        | Group exp -> evaluate exp
        | Constant n -> n
