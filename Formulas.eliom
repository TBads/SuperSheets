(* TODO: Need to handle function calls with >2 args *)
(* TODO: Need to re-evaluate downstream cells when an upstream cell changes value *)

(* NOTE: For now, only allow functions to take two arguments *)
{client{

  type formula_result =
    | Num of float
    | Txt of string
    | Err of string

  let func_hashtbl = Hashtbl.create 10

  let () =
    Hashtbl.add func_hashtbl "min"  (fun x y -> if x < y then x else y);
    Hashtbl.add func_hashtbl "max"  (fun x y -> if x > y then x else y)

  let available_function_names = Hashtbl.fold (fun key value acc -> key :: acc) func_hashtbl []

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
    | Function of string
    | CellRef
    | Comma
    | LeftParen
    | RightParen
    | StringTokenError of string

  let string_of_formula_result =
    function
    | Num f -> string_of_float f
    | Txt t -> t
    | Err s -> s

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
    | "," -> Comma
    | "cell" -> CellRef
    | _  ->
      try
        if List.mem s available_function_names
        then Function s
        else Number (float_of_string s)
      with
        _ -> StringTokenError s

  let string_of_token =
    function
    | FormulaBegin -> "FormulaBegin"
    | Number n -> "Number" ^ (string_of_float n)
    | Op Add -> "Add"
    | Op Subtract -> "Subtract"
    | Op Multiply -> "Multiply"
    | Op Divide -> "Divide"
    | Op Exp -> "Exp"
    | Function s -> "Function " ^ s
    | Comma -> ","
    | LeftParen -> "LeftParen"
    | RightParen -> "RightParen"
    | CellRef -> "Cell"
    | StringTokenError s -> ("StringTokenError " ^ s)

  let val_of_cell_ref r c =
    let open Types in
    try
      match Hashtbl.find Types.h (r, c) with
      | SingleCell sc ->
          (try Number (float_of_string sc.txt) with _ -> StringTokenError "Error: val_of_cell_ref")
      | MergedCell mc ->
          (try Number (float_of_string mc.txt) with _ -> StringTokenError "Error: val_of_cell_ref")
    with
      Not_found -> (
        let msg =
          "Error: Cell (" ^ (string_of_int r) ^ ", " ^ (string_of_int c) ^ ") has no value!"
        in
        print_endline msg;
        failwith msg
      )

  (* Step 1 - Split the string into its component characters *)
  let rec string_list_of_string ?(acc = []) s =
    if String.length s > 0
    then
      string_list_of_string ~acc:((String.sub s 0 1) :: acc) (String.sub s 1 (String.length s - 1))
    else List.rev acc

  let rec reverse_string ?(acc = "") s =
    if String.length s > 0
    then reverse_string ~acc:(String.sub s 0 1 ^ acc) (String.sub s 1 (String.length s - 1))
    else acc

  (* Step 2 - Recombine based on the separators *)
  let rec recombine ?(acc : string list = []) sl =
    let separators = ["="; "+"; "-"; "*"; "/"; "("; ")"; "^"; ","] in
    match acc, sl with
    | _, [] -> List.map (reverse_string) acc |> List.rev
    | [], h :: t -> recombine ~acc:[h] t
    | hd :: tl, h :: t -> (
        if List.mem hd separators || List.mem h separators
        then recombine ~acc:(h :: acc) t
        else recombine ~acc:((h ^ hd) :: tl) t
      )

  let rec remove_whitespace ?(acc = []) sl =
    match sl with
    | [] -> List.rev acc
    | hd :: tl ->
      if hd = " "
      then remove_whitespace ~acc tl
      else remove_whitespace ~acc:(hd :: acc) tl

  (* Parse a string into a list of strings that will be evaluated as tokens *)
  let parse_string s =
    string_list_of_string s
    |> remove_whitespace
    |> recombine
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

  let eval_cell_ref (l : token list) =
    match l with
    | LeftParen :: Number r :: Comma :: Number c :: RightParen :: tl ->
        (val_of_cell_ref (int_of_float r) (int_of_float c), tl)
    | _ -> failwith "Error: Calling values from other cell must take the form cell(row, column)!"

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

    let rec handle_comma () =
      print_endline "handle_comma:";
      print_stack op_stack;
      try
        if Stack.top op_stack = LeftParen
        then (print_endline "Stack.top op_stack = LeftParen")
        else(
          print_endline "Stack.top op_stack != LeftParen";
          Queue.push (Stack.pop op_stack) out_que;
          handle_comma ()
        )
      with
        _ -> failwith "Error: handle_comma: Unmatched parentheses"
    in

    let rec run_algo tokens =
      match tokens with
      | [] -> (try (Queue.push (Stack.pop op_stack) out_que; run_algo tokens) with empty -> ())
      | FormulaBegin :: tl -> run_algo tl
      | Number n :: tl -> (Queue.push (Number n) out_que; run_algo tl)
      | Op Add :: tl -> (
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
      | Op Multiply :: tl -> (
          try
            let o2 = Stack.top op_stack in
            if o2 = (Op Multiply) || o2 = (Op Divide)
            then (Queue.push (Stack.pop op_stack) out_que; run_algo tokens)
            else (Stack.push (Op Multiply) op_stack; run_algo tl)
          with
            empty -> (Stack.push (Op Multiply) op_stack; run_algo tl)
        )
      | Op Divide :: tl -> (
          try
            let o2 = Stack.top op_stack in
            if o2 = (Op Multiply) || o2 = (Op Divide)
            then (Queue.push (Stack.pop op_stack) out_que; run_algo tokens)
            else (Stack.push (Op Divide) op_stack; run_algo tl)
          with
            empty -> (Stack.push (Op Divide) op_stack; run_algo tl)
        )
      | Op Exp :: tl -> (Stack.push (Op Exp) op_stack; run_algo tl)
      | Function f :: tl -> (Stack.push (Function f) op_stack; run_algo tl)
      | Comma :: tl -> (
          print_endline "Comma :: tl";
          (*print_stack op_stack;
          failwith "Comma :: tl"*)
          handle_comma ();
          run_algo tl
        )
      | LeftParen :: tl -> (
          Stack.push LeftParen op_stack;
          run_algo tl
        )
      | RightParen :: tl -> (
          handle_right_paren ();
          run_algo tl
        )
      | CellRef :: tl -> (
          let eval_result, tl2 = eval_cell_ref tl in
          Queue.push (eval_result) out_que;
          run_algo tl2
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
      | Function f :: tl -> (
          let n1 = float_of_token @@ Stack.pop s in
          let n2 = float_of_token @@ Stack.pop s in
          let result = (Hashtbl.find func_hashtbl f) n1 n2 in
          Stack.push (Number result) s;
          run tl
        )
      | _ -> failwith "Error: eval"
    in

    run l;
    Stack.pop s

  let eval_string s =
    let l = parse_string s in
    try
      if List.hd l = FormulaBegin
      then (match eval @@ shunting_yard l with | Number n -> (string_of_float n) | _ -> s)
      else s
    with
    | Failure msg -> msg
    | _ -> s

}}
