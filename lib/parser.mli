type bin_operator =
        | Add
        | Sub
        | Mult
        | Div
        | Pow

type expression =
        | Constant of float
        | Binary of bin_operator * expression * expression
        | Group of expression


type parser = {
        previous: Lexer.token list;
        current: Lexer.token;
        next: Lexer.token list;
}

val new_parser : Lexer.token list -> parser

val parse : parser -> expression
