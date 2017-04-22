open Types
open Mysql
open Lwt.Infix

(* Database *)
let user_db = Config.user_db

exception Db_Error of string

let string_of_option so =
  match so with
  | Some s -> s
  | None -> ""

let sll_of_res res =
  Mysql.map res (fun a -> Array.to_list a)
  |> List.map (List.map string_of_option)

(* Check if the username already exists in database *)
let username_exists new_username =
  let conn = connect user_db in
  let sql_stmt =
    "SELECT username FROM SS.users WHERE username = '" ^
    (real_escape conn new_username) ^ "'"
  in
  let query_result = exec conn sql_stmt in
  disconnect conn
  |> fun () -> if (size query_result) = Int64.zero then Lwt.return false else Lwt.return true

(* Check if the email_address already exists in the database *)
let email_exists new_email =
  let conn = connect user_db in
  let sql_stmt =
    "SELECT email FROM SS.users WHERE email = '" ^ (real_escape conn new_email) ^ "'"
  in
  let query_result = exec conn sql_stmt in
  disconnect conn
  |> fun () -> if (size query_result) = Int64.zero then Lwt.return false else Lwt.return true

(* TODO: Only allow a password of max length 32 (?) characters <-- add this to password check fun *)
(* Connect, write a new user to the a database and disconnect *)
(* write_new_user : Types.user -> string *)
let write_new_user (u : user) pwd =
  match u.username with
  | Some un ->
      (
        let conn = connect user_db in
        let esc s = Mysql.real_escape conn s in
        username_exists un
        >>= fun b ->
          (
            if b then (disconnect conn |> fun () -> Lwt.return @@ "Username already exists")
            else
              let g = string_of_option in
              (* Salt and hash the password before storing *)
              let pwd' =
                Bcrypt.hash ~count:Config.pwd_iter_count (esc pwd)
                |> Bcrypt.string_of_hash
              in
              let sql_stmt =
                "INSERT INTO SS.users (username, email, password)" ^
                " VALUES('" ^ (esc @@ g u.username) ^ "', '" ^ (esc @@ g u.email) ^ "', '" ^
                (esc pwd') ^ "')"
              in
              let _ = exec conn sql_stmt in
              disconnect conn |> fun () -> Lwt.return "Username successfully created"
          )
      )
  | None -> Lwt.return "No username found"

(* Verify a username and password pair *)
let verify_login username pwd =
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  let sql_stmt =
    "SELECT username, password FROM SS.users WHERE username = '" ^ (esc username) ^"'"
  in
  let query_result = exec conn sql_stmt in
  let name_pass =
    try query_result |> sll_of_res |> List.hd
    with Failure hd -> ["username fail"; "password fail"]
  in
  let verified =
    try
      List.nth name_pass 0 = username &&
      Bcrypt.verify (esc pwd) (Bcrypt.hash_of_string @@ esc @@ List.nth name_pass 1)
    with Bcrypt.Bcrypt_error -> false
  in
  disconnect conn;
  Lwt.return verified

(* Check that a password meets complexity requirements *)
(* pwd_req_check -> string -> bool * string *)
let pwd_req_check pwd =
   (* At least 8 characters *)
   let length_check = if String.length pwd >= 8 then true else false in
   let length_msg =
     if length_check
     then ""
     else "The password must contain at least 8 characters."
   in
   (* At least 1 uppercase letter *)
   (*let uppercase_check =
     try (Str.search_forward (Str.regexp "[A-Z]") pwd 0) >= 0 with
     | Not_found -> false
   in
   let uppercase_msg =
     if uppercase_check
     then ""
     else "The password must contain at lease 1 uppercase character."
   in
   (* At least 3 numbers *)
   let number_check =
     Str.bounded_full_split (Str.regexp "[0-9]") pwd 0
     |> List.filter (fun x -> match x with Str.Delim _ -> true | _ -> false)
     |> List.length >= 3
   in
   let number_msg =
     if number_check
     then ""
     else "The password must contain at least 3 numbers."
     in*)
   (* Less than 100 characters *)
   let max_len_check = if String.length pwd <= 100 then true else false in
   let max_len_msg =
     if max_len_check
     then ""
     else "The password length must not contain more than 100 characters."
   in
   (* No Spaces Allowed *)
   let spaces_check =
     try if (Str.search_forward (Str.regexp " ") pwd 0) >= 0 then false else true with
     | Not_found -> true
   in
   let spaces_msg =
     if spaces_check
     then ""
     else "The password cannot contain any spaces."
   in

   match length_check, (*uppercase_check, number_check,*) max_len_check, spaces_check with
   | true, (*true, true,*) true, true -> true, ""
   | _, (*_, _,*) _, _ ->
     false, ("Error: " ^ length_msg ^ (*uppercase_msg ^ number_msg ^*) max_len_msg ^ spaces_msg)

let user_number (username : string) =
  print_string "\n\nuser_number\n\n";
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  let sql_stmt =
    "SELECT user_number FROM SS.users WHERE username = '" ^ (esc username) ^ "'"
  in
  print_string sql_stmt;
  match exec conn sql_stmt |> fetch with
  | Some a -> (
      disconnect conn;
      string_of_option a.(0)
      |> int_of_string
      |> fun i -> Some i
    )
  | None -> (disconnect conn; None)

(* Check if the sheet already exists in database *)
let sheet_exists user_number sheet_name =
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  let sql_stmt =
    "SELECT sheet_name FROM SS.sheets " ^
    "WHERE user_number = '" ^ (esc @@ string_of_int user_number) ^
    "' AND sheet_name = '" ^ (esc sheet_name) ^ "'"
  in
  let query_result = exec conn sql_stmt in
  disconnect conn;
  if (size query_result) = Int64.zero
  then Lwt.return false
  else Lwt.return true

let write_new_sheet (username : string) sheet_name sheet_data =
  Lwt_io.print "\n\nwrite_new_sheet:\n\n" >>
  (* TODO Pick back up here, something is not working right here *)
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  match user_number username with
  | None -> (
      Lwt_io.print "Error: write_new_sheet - user number not found" >>
      Lwt.return "Error: Cannot save data! Please contact administrators."
    )
  | Some n -> (
      Lwt_io.print "write_new_sheet - user number found\n" >>
      let sql_stmt =
        "INSERT INTO SS.sheets (user_number, sheet_name, sheet_data)" ^
        " VALUES('" ^ (esc @@ string_of_int n) ^ "', '" ^ (esc sheet_name) ^ "', '" ^
        (esc sheet_data) ^ "')"
      in
      Lwt_io.print ("\n\nsql_stmt = " ^ sql_stmt) >>
      let _ = exec conn sql_stmt in
      disconnect conn;
      Lwt_io.print "Success: write_new_sheet - Data Saved" >>
      Lwt.return "Data Saved"
    )

let update_existing_sheet (username : string) sheet_name sheet_data =
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  match user_number username with
  | None -> Lwt.return "Error: Cannot save data! Please contact administrators."
  | Some n -> (
      let sql_stmt =
        "UPDATE SS.sheets SET sheet_data = '" ^ (esc sheet_data) ^
        "' WHERE user_number= '" ^ (esc @@ string_of_int n) ^
        "' AND sheet_name = '" ^ (esc sheet_name) ^ "'"
      in
      let _ = exec conn sql_stmt in
      disconnect conn;
      Lwt.return "Data Saved"
    )

(* If the sheet exists then update it, otherwise write a new sheet *)
(* TODO: need to add a check that the user is verified first *)
let save_or_update_sheet ((username : string), (sheet_name : string), (sheet_data : string)) =
  Lwt_io.print "\nsave_or_update_sheet\n" >>
  try_lwt (
    match user_number username with
    | Some n -> (
        Lwt_io.print ("\nsave_or_update_sheet  Some " ^ (string_of_int n) ^ "\n\n") >>
        lwt exists = sheet_exists n sheet_name in
        if exists
        then (
          Lwt_io.print "\n\nExists\n\n" >>
          update_existing_sheet username sheet_name sheet_data
        )
        else (
          Lwt_io.print "\n\nNot Exists\n\n" >>
          write_new_sheet username sheet_name sheet_data
        )
      )
    | _ -> Lwt.return "Error: save_or_update_sheet - no user number found"
  ) with
  | Db_Error s -> Lwt.return s
  | Failure s -> Lwt.return s
  | _ -> Lwt.return "ERROR: save_or_update_sheet"

{shared{
  open Deriving_Json

  (* TODO: Maybe change user to be a username by verified bool? *)
  type save_or_update_arg = (string * string * string) deriving (Json)

}}

{server{
  let save_or_update_sheet' = server_function Json.t<save_or_update_arg> save_or_update_sheet
}}

(* TODO: need to check if the user has been verified *)
let user_sheets username =
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  match user_number username with
  | None -> (disconnect conn; [])
  | Some un -> (
      let sql_stmt =
        "SELECT sheet_name from SS.sheets " ^
        "WHERE user_number = '" ^ (esc @@ string_of_int un) ^ "'"
      in
      let query_result = exec conn sql_stmt in
      disconnect conn;
      sll_of_res query_result |> List.map List.hd
    )
