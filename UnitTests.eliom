(* Unit Tests *)

{client{
  open Cells
  open Dom
  open Dom_html

  (*** Setup the Environment for Testing ***)

  let set_selected_cell () =
    selected_cell := Some {
      row = 2;
      col = 2;
      id  = "2_2";
      txt = "jk"
    }

  let set_shift_area () =
    shift_area := Some [
      {row = 2; col = 2; id = "2_2"; txt = "jk"};
      {row = 2; col = 3; id = "2_3"; txt = "holla"};
      {row = 3; col = 2; id = "3_2"; txt = "wassup"};
      {row = 3; col = 3; id = "3_3"; txt = "hey hey hey"}
      ];
    match !shift_area with
    | None -> ()
    | Some sa ->
      List.iter (fun (c : cell) ->
      let body = document##body in
      let td = createTd document in
      td##id <- Js.string c.id;
      td##textContent <- Js.some (Js.string c.txt);
      (* TODO: Build a full table first, the append the cells to the fresh table *)
      appendChild document##body td
    ) sa

  (*** Define Tests ***)

  type test_result = {
    name : string;
    pass : bool
  }

  let row_of_id_test = {
    name = "row_of_id_test";
    pass = (row_of_id "2_21") = 2
  }

  let col_of_id_test = {
    name = "col_of_id_test";
    pass = (col_of_id "2_17") = 17
  }

  let key_of_id_test = {
    name = "key_of_id_test";
    pass = (key_of_id "21_43") = (21, 43)
  }

  (*let cell_of_id_test =
    set_shift_area ();
    {
      name = "cell_of_id_test";
      pass = (cell_of_id "2_3") = Some {row = 2; col = 3; id = "2_3"; txt = "holla"}
    }*)

  let tests = [
    row_of_id_test;
    col_of_id_test;
    key_of_id_test
    (*cell_of_id_test*)
  ]

  (* Append the test results to the page *)
  let append_result (tr : test_result) =
    let body = document##body in
    let test_msg = createP document in
    let () =
      match tr.pass with
      | true -> test_msg##textContent  <- Js.some (Js.string (tr.name ^ " passed"))
      | false -> test_msg##textContent <- Js.some (Js.string ("*** " ^ tr.name ^ " FAILED ***"))
    in
    appendChild body test_msg

  let run_tests () =
    let all_tests_pass = List.map (fun tr -> tr.pass) tests |> fun l -> not (List.mem false l) in
    match all_tests_pass with
    | true  -> append_result {name = "ALL TESTS"; pass = true}
    | false -> List.iter (append_result) tests

}}
