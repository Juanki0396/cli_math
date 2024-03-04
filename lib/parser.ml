open Lexer

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
        previous: token list;
        current: token;
        next: token list;
}

let new_parser tok_lst = 
        match tok_lst with
        | hd :: tl -> {previous=[]; current = hd; next = tl}
        | _ -> raise (Invalid_argument "Empty list")

let advance p =
        match p.next with
        | [] -> p
        | hd :: tl -> {
                previous = p.current :: p.previous;
                current = hd;
                next = tl
        }

let back p =
        match p.previous with
        | [] -> p
        | hd :: tl -> {
                previous = tl;
                current = hd;
                next = p.current :: p.next;
        }

let n_move f n p = 
        let rec aux n p =
                match n with 
                | 0 -> p
                | a when a < 0 -> p
                | _ -> aux (n-1) (f p)
        in
        aux n p

let get_prev p =
        let len = List.length p.previous in
        let pback = back p in 
        let new_p = { previous=pback.previous; current=pback.current; next = [] } in
        n_move back len new_p

let get_next p =
        let padv = advance p in 
        { previous=[]; current=padv.current; next = padv.next }

let get_group p =
        let pnext = get_next p in
        let rec aux n p = 
                match n, p.current with
                | 0, _ -> get_prev p 
                | _, Parenthesis_r -> aux (n-1) (advance p)
                | _, Parenthesis_l -> aux (n+1) (advance p)
                | _, _ -> aux n (advance p)
        in
        aux 1 pnext

let rec find_binary_add p =
        let {current; next; _} = p in
        match (current, next) with
        | (Addition, _ ) -> Some (Add,p)
        | (Substraction, _) -> Some (Sub,p)
        | (_, []) -> None
        | _ -> find_binary_add (advance p)

let rec find_binary_mul p =
        let {current; next; _} = p in
        match (current, next) with
        | (Multiplication, _) -> Some (Mult, p)
        | (Division, _) -> Some (Div, p)
        | (_, []) -> None
        | _ -> find_binary_mul (advance p)

let rec find_binary_exp p =
        let {current; next; _} = p in
        match (current, next) with
        | (Exponetiation, _) -> Some (Pow, p)
        | (_, []) -> None
        | _ -> find_binary_exp (advance p)

let rec find_group p = 
        let {current; next; _} = p in
        match (current, next) with
        | (Parenthesis_l, _) -> Some p
        | (_, []) -> None
        | _ -> find_group (advance p)

exception ParsingError

let rec parse p =
        let add = find_binary_add p in
        let mul = find_binary_mul p in
        let exp = find_binary_exp p in
        let group = find_group p in
        match (add, mul, exp, group, p.current) with
        | (Some (op,p), _ , _ , _ , _) -> Binary (op, parse (get_prev p), parse (get_next p))
        | (None , Some (op,p) , _ , _ , _) -> Binary (op, parse (get_prev p), parse (get_next p))
        | (None , None, Some (op,p) , _ , _) -> Binary (op, parse (get_prev p), parse (get_next p))
        | (None , None, None, Some p, _ ) -> Group (parse (get_group p))
        | (None , None, None, None , Number n ) -> Constant (Float.of_string n)
        | (None , None, None, None , _ ) -> print_token p.current; raise ParsingError

