open Cli_math

let example = "6 + 4 / 2"

let ast =
        match Lexer.scan_token example with
        | Ok tok_lst -> Parser.parse (Parser.new_parser tok_lst)
        | Error _ -> raise (Invalid_argument "Invalid input")

let n = Eval.evaluate ast

let () = Printf.printf "%s = %f" example n
