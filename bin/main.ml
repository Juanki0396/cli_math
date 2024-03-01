let example = "1 - 2 * 34 + (8-1)"

type operator = 
        | Addition
        | Substraction
        | Multiplication
        | Division

type token =
        | Number of float
        | Parenthesis_l
        | Parenthesis_r
        | Operator of operator
        | Identificator of string

let extract_fst_num str =
        let rec aux acc s =
                let next = String.get s 0 in
                let len = String.length str in
                let substr = String.sub str 1 (len - 1) in
                match next with
                | '0'..'9' -> aux (acc ^ String.make 1 next) substr
                | '.' when Bool.not (String.contains acc '.') -> aux (acc ^ String.make 1 next) substr
                | _ -> (substr, Float.of_string (acc ^ String.make 1 next))
        in
        aux "" str

let () = print_float (snd (extract_fst_num example))
