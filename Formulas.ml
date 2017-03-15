(* TODO: Need some unit tests for this file, not sure the ast is being build 100% correctly *)

type string_token =
  | Formula
  | Number of float
  | Add
  | Subtract
  | Multiply
  | Divide
  | StringTokenError of string

type ast_token =
  | FormulaBegin of ast_token
  | Number of float
  | Add of ast_token * ast_token
  | Subtract of ast_token * ast_token
  | Multiply of ast_token * ast_token
  | Divide of ast_token * ast_token
  | AstTokenError of string

type eval_result =
  | Text of string
  | Number of string
  | Error of string

let string_of_string_token =
  function
  | Formula -> "Formula("
  | Number n -> ("Number " ^ (string_of_float n))
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | StringTokenError s -> "StringTokenError: " ^ s

let token_of_string s =
  match s with
  | "=" -> Formula
  | "+" -> Add
  | "-" -> Subtract
  | "*" -> Multiply
  | "/" -> Divide
  | _   -> try Number (float_of_string s) with | _ -> StringTokenError s

(* Parse a string into a list of strings that will be evaluated as tokens *)
let parse_string s =
  Str.split (Str.regexp "[ \t]+") s
  |> List.map (Str.full_split (Str.regexp "[= + /- - /* //]+"))
  |> List.flatten
  |> List.map (function | Str.Delim x -> x | Str.Text x -> x)
  |> List.map token_of_string

(* Build an AST from a string list *)
let rec build_ast ?(token_stack = []) (l : string_token list) =
  match token_stack, l with
  | [], [] -> AstTokenError "Not a formula"
  | h :: t, [] -> h
  | [], Formula :: tl -> FormulaBegin (build_ast tl)
  | h :: t, Add :: tl -> Add (h, build_ast tl)
  | h :: t, Subtract :: tl -> Subtract (h, build_ast tl)
  | h :: t, Multiply :: Number n :: tl ->
      build_ast ~token_stack:(Multiply (h,  Number n) :: token_stack) tl
   | h :: t, Divide :: Number n :: tl ->
      build_ast ~token_stack:(Divide (h,  Number n) :: token_stack) tl
  | [], Number n :: tl -> build_ast ~token_stack:(Number n :: token_stack) tl
  | _, _ -> failwith "Error: build_ast"

let rec eval_ast ast =
  match ast with
  | FormulaBegin x -> eval_ast x
  | (Number n) -> n
  | (Add (x, y)) -> eval_ast x +. eval_ast y
  | (Subtract (x, y)) -> eval_ast x -. eval_ast y
  | (Multiply (x, y)) -> eval_ast x *. eval_ast y
  | (Divide (x, y)) -> eval_ast x /. eval_ast y
  | AstTokenError s -> failwith s

let eval_string s =
  try (
    if String.sub s 0 1 = "="
    then (
      parse_string s
      |> build_ast
      |> eval_ast
      |> fun x -> Number (string_of_float x)
    )
    else
      Text s
  )
  with
  | Invalid_argument _ -> Text s
  | _ -> Error "Error: eval_string: Dont know how to handle that."
