open Printf

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

let print_token ?(before="") ?(after="") ?(newline=true) t =
        let after = if newline then after ^ "\n" else after in
       match t with
        | Number str -> printf "%s%s%s%s " before "Number " str after
        | Parenthesis_l -> printf "%s%s%s" before "Parenthesis_l" after
        | Parenthesis_r -> printf "%s%s%s" before "Parenthesis_r" after
        | Addition -> printf "%s%s%s" before "Addition" after
        | Substraction -> printf "%s%s%s" before "Substraction" after
        | Multiplication -> printf "%s%s%s" before "Multiplication" after
        | Division -> printf "%s%s%s" before "Division" after
        | Exponetiation -> printf "%s%s%s" before "Exponetiation" after
        | Equality -> printf "%s%s%s" before "Equality" after
        | Identificator str -> printf "%s%s%s%s" before "Identificator " str after

let advance str = String.sub str 1 ((String.length str) - 1)

let extract str = String.get str 0

let rec get_num acc str = 
        if String.empty <> str then 
                let c = extract str in
                match c with 
                | '0' .. '9' -> get_num (acc ^ (String.make 1 c)) (advance str)
                | '.' when not (String.contains acc '.')  -> get_num (acc ^ (String.make 1 c)) (advance str)
                | '.' when (String.contains acc '.') -> Error "Number cannot have more than one '.'"
                | _ -> Ok (acc, str)
        else
                Ok (acc, str)

let rec get_word acc str = 
        if String.empty <> str then 
                let c = extract str in
                match c with 
                | 'a' .. 'z' | 'A' .. 'Z' | '_' -> get_word (acc ^ (String.make 1 c)) (advance str)
                | _ -> (acc, str)
        else
                (acc, str)

let scan_token str = 
        let rec aux acc str =
                if String.empty <> str then 
                        match extract str with
                        | ' ' -> aux acc (advance str)
                        | '(' -> aux (Parenthesis_l :: acc) (advance str)
                        | ')' -> aux (Parenthesis_r :: acc) (advance str)
                        | '=' -> aux (Equality :: acc) (advance str)
                        | '+' -> aux (Addition :: acc) (advance str)
                        | '-' -> aux (Substraction :: acc) (advance str)
                        | '*' -> aux (Multiplication :: acc) (advance str)
                        | '/' -> aux (Division :: acc) (advance str)
                        | '^' -> aux (Exponetiation :: acc) (advance str)
                        | 'a'..'z' | 'A' .. 'Z' -> 
                                ( let (id, str) = get_word "" str in
                                        aux (Identificator id :: acc) str)
                        | '0'..'9' -> 
                                (match get_num "" str with
                                        | Ok (n, str) -> aux ((Number n) :: acc) str 
                                        | Error e -> Error e)
                        | _ -> Error "Not valid character"
                else 
                        Ok (List.rev acc)
        in
        aux [] str
