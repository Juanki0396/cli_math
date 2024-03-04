type token =
        | Number of string
        | Parenthesis_l
        | Parenthesis_r
        | Addition 
        | Substraction 
        | Multiplication 
        | Division 
        | Exponetiation
        | Equality
        | Identificator of string

val print_token : ?before:string -> ?after:string -> ?newline:bool -> token -> unit
val scan_token : string -> (token list, string) result

