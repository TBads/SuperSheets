(* TODO: Need to be fully handle cell references and function calls *)

type operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Exp

type token =
  | FormulaBegin
  | Number of float
  | Op of operator
  | LeftParen
  | RightParen
  | StringTokenError of string

let token_of_string s =
  match s with
  | "=" -> FormulaBegin
  | "+" -> Op Add
  | "-" -> Op Subtract
  | "*" -> Op Multiply
  | "/" -> Op Divide
  | "^" -> Op Exp
  | "(" -> LeftParen
  | ")" -> RightParen
  | _   -> try Number (float_of_string s) with _ -> StringTokenError s

let string_of_token =
  function
  | FormulaBegin -> "FormulaBegin"
  | Number n -> "Number" ^ (string_of_float n)
  | Op Add -> "Add"
  | Op Subtract -> "Subtract"
  | Op Multiply -> "Multiply"
  | Op Divide -> "Divide"
  | Op Exp -> "Exp"
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | StringTokenError s -> ("StringTokenError " ^ s)

(* Parse a string into a list of strings that will be evaluated as tokens *)
let parse_string s =
  Str.split (Str.regexp "[ \t]+") s
  |> List.map (Str.full_split (Str.regexp "[= + /- - /* // ( ) ^]"))
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

let print_stack s =
  let printable_stack = Stack.copy s in

  let rec print_items () =
    try
      print_endline (string_of_token @@ Stack.pop printable_stack);
      print_items ()
    with _ -> print_newline ()
  in

  print_endline "Stack =";
  print_items ()

(* Shunting-yard algorithm *)
let shunting_yard (l : token list) =

  let out_que, op_stack = Queue.create (), Stack.create () in

  let rec handle_right_paren () =
    try
      if Stack.top op_stack = LeftParen
      then (
        let _ = Stack.pop op_stack in
        ()
      )
      else (
        Queue.push (Stack.pop op_stack) out_que;
        handle_right_paren ()
      )
    with empty -> failwith "Error: Unmatched parentheses"
  in

  let rec run_algo tokens =
    match tokens with
    | [] -> (try (Queue.push (Stack.pop op_stack) out_que; run_algo tokens) with empty -> ())
    | FormulaBegin :: tl -> run_algo tl
    | Number n :: tl -> (Queue.push (Number n) out_que; run_algo tl)
    | Op Add as o1 :: tl -> (
        try
          let o2 = Stack.top op_stack in
          match o2 with
          | Op _ -> (
              let _ = Stack.pop op_stack in
              Queue.push o2 out_que;
              run_algo tokens
            )
          | _ -> (Stack.push (Op Add) op_stack; run_algo tl)
        with
          empty -> (Stack.push (Op Add) op_stack; run_algo tl)
      )
    | Op Subtract :: tl -> (
        try
          let o2 = Stack.top op_stack in
          match o2 with
          | Op _ -> (
              let _ = Stack.pop op_stack in
              Queue.push o2 out_que;
              run_algo tokens
            )
          | _ -> (Stack.push (Op Subtract) op_stack; run_algo tl)
        with
          empty -> (Stack.push (Op Subtract) op_stack; run_algo tl)
      )
    | Op Multiply as o1 :: tl -> (
        try
          let o2 = Stack.top op_stack in
          if o2 = (Op Multiply) || o2 = (Op Divide)
          then (Queue.push (Stack.pop op_stack) out_que; run_algo tokens)
          else (Stack.push (Op Multiply) op_stack; run_algo tl)
        with
          empty -> (Stack.push (Op Multiply) op_stack; run_algo tl)
      )
    | Op Divide as o1 :: tl -> (
        try
          let o2 = Stack.top op_stack in
          if o2 = (Op Multiply) || o2 = (Op Divide)
          then (Queue.push (Stack.pop op_stack) out_que; run_algo tokens)
          else (Stack.push (Op Divide) op_stack; run_algo tl)
        with
          empty -> (Stack.push (Op Divide) op_stack; run_algo tl)
      )
    | Op Exp as o1 :: tl -> (Stack.push (Op Exp) op_stack; run_algo tl)
    | LeftParen :: tl -> (
        Stack.push LeftParen op_stack;
        run_algo tl
      )
    | RightParen :: tl -> (
        handle_right_paren ();
        run_algo tl
      )
    | _ -> failwith "Error: shunting_yard"
  in

  run_algo l;
  List.rev @@ list_of_que out_que

let float_of_token =
  function
  | Number n -> n
  | _ -> failwith "Error: float_of_token"

(* Evaluate a stack of tokens in reverse polish notation *)
let eval l =
  let (s : token Stack.t) = Stack.create () in

  let rec run tokens =
    match tokens with
    | [] -> ()
    | Number n :: tl -> (
        (Stack.push (Number n) s; run tl)
      )
    | Op Add :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n1 +. n2)) s;
        run tl
      )
    | Op Subtract :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n2 -. n1)) s;
        run tl
      )

    | Op Multiply :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n1 *. n2)) s;
        run tl
      )
    | Op Divide :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        Stack.push (Number (n2 /. n1)) s;
        run tl
      )
    | Op Exp :: tl -> (
        let n1 = float_of_token @@ Stack.pop s in
        let n2 = float_of_token @@ Stack.pop s in
        print_endline ("n1 = " ^ string_of_float n1 ^ ", n2 = " ^ string_of_float n2);
        Stack.push (Number (n2 ** n1)) s;
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
