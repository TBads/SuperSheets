open Types
open Mysql
open Lwt.Infix

(* Database *)
let user_db = Config.user_db

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
