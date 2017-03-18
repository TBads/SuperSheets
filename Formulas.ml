(* TODO: Need to be fully handle parentheses, exponents, cell references and function calls *)

type token =
  | FormulaBegin
  | Number of float
  | Add
  | Subtract
  | Multiply
  | Divide
  | LeftParen
  | RightParen
  | StringTokenError of string

let token_of_string s =
  match s with
  | "=" -> FormulaBegin
  | "+" -> Add
  | "-" -> Subtract
  | "*" -> Multiply
  | "/" -> Divide
  (*| "(" -> LeftParen
    | ")" -> RightParen*)
  | _   -> try Number (float_of_string s) with _ -> StringTokenError s

let string_of_token =
  function
  | FormulaBegin -> "FormulaBegin"
  | Number n -> "Number" ^ (string_of_float n)
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | StringTokenError s -> ("StringTokenError " ^ s)

(* Parse a string into a list of strings that will be evaluated as tokens *)
let parse_string s =
  Str.split (Str.regexp "[ \t]+") s
  |> List.map (Str.full_split (Str.regexp "[= + /- - /* //]+"))
  |> List.flatten
  |> List.map (function | Str.Delim x -> x | Str.Text x -> x)
  |> List.map token_of_string

let rec list_of_que ?(acc = []) q =
  try
    list_of_que ~acc:(Queue.pop q :: acc) q
  with
    empty -> acc

let stack_of_list l =
  let s = Stack.create () in

  let rec push_to_stack xs =
    match xs with
    | [] -> s
    | hd :: tl -> Stack.push hd s; push_to_stack tl
  in

  push_to_stack l

let stack_of_que q =
  list_of_que q |> stack_of_list

(* Shunting-yard algorithm *)
let shunting_yard (l : token list) =

  let out_que, op_stack = Queue.create (), Stack.create () in

  (* TODO: handling of parentheses *)
  let rec run_algo tokens =
    match tokens with
    | [] -> (try (Queue.push (Stack.pop op_stack) out_que; run_algo tokens) with empty -> ())
    | FormulaBegin :: tl -> run_algo tl
    | Number n :: tl -> (
        Queue.push (Number n) out_que;
        run_algo tl
      )
    | Add :: tl -> (
        try
          let o2 = Stack.pop op_stack in
          Queue.push o2 out_que;
          run_algo tokens
        with
          empty -> (Stack.push Add op_stack; run_algo tl)
      )
    | Subtract :: tl -> (
        try
          let o2 = Stack.pop op_stack in
          Queue.push o2 out_que;
          run_algo tokens
        with
          empty -> (Stack.push Subtract op_stack; run_algo tl)
      )
    | Multiply as o1 :: tl -> (
        try
          let o2 = Stack.top op_stack in
          if o2 = Multiply || o2 = Divide
          then (Queue.push (Stack.pop op_stack) out_que; run_algo tokens)
          else (Stack.push Multiply op_stack; run_algo tl)
        with
          empty -> (Stack.push Multiply op_stack; run_algo tl)
      )
    | Divide as o1 :: tl -> (
        try
          let o2 = Stack.top op_stack in
          if o2 = Multiply || o2 = Divide
          then (Queue.push (Stack.pop op_stack) out_que; run_algo tokens)
          else (Stack.push Divide op_stack; run_algo tl)
        with
          empty -> (Stack.push Divide op_stack; run_algo tl)
      )

    (*| LeftParen :: tl -> (Stack.push LeftParen op_stack; run_algo tl)
    | RightParen :: tl ->
      if Stack.top op_stack = LeftParen
      then (Queue.push (Stack.pop op_stack) out_que; run_algo tl)
      else (Queue.push (Stack.pop op_stack) out_que; run_algo tokens)*)
    | _ -> failwith "Error: shunting_yard"
  in

  run_algo l;
  List.rev @@ list_of_que out_que

let float_of_token =
  function
  | Number n -> n
  | _ -> failwith "Error: float_of_token"

let print_stack s =
  let printable_stack = Stack.copy s in

  let rec print_items () =
    try
      print_endline (string_of_token @@ Stack.pop printable_stack);
      print_items ()
    with _ -> print_newline ()
  in

  print_items ()

(* Evaluate a stack of tokens in reverse polish notation *)
let eval l =
  let (s : token Stack.t) = Stack.create () in

  let rec run tokens =
    match tokens with
    | [] -> ()
    | Number n :: tl -> (
        (Stack.push (Number n) s; run tl)
      )
    | Add :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n1 +. n2)) s;
        run tl
      )
    | Subtract :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n2 -. n1)) s;
        run tl
      )

    | Multiply :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n1 *. n2)) s;
        run tl
      )
    | Divide :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n2 /. n1)) s;
        run tl
      )
    | _ -> failwith "Error: eval"
  in

  run l;
  Stack.pop s

let eval_string s =
  parse_string s
  |> shunting_yard
  |> eval
