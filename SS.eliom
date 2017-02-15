{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

module SS_app =
  Eliom_registration.App (
    struct
      let application_name = "SS"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

{server{

   let shell_print = server_function Json.t<string> Lwt_io.print
   (* TODO: Need a function to write the string to disc *)

}}

{client{

  open Dom
  open Dom_html

  (* Keys take the form of row_col, ex. row 1 column 3 has the key "1_3" *)
  let h : (string, string) Hashtbl.t = Hashtbl.create 100

  let store_cell (key : string) (value : string) =
    if Hashtbl.mem h key
    then Hashtbl.replace h key value
    else Hashtbl.add h key value

  let get_cell key = Hashtbl.find h key

  (* TODO: Maybe this should be JSON? *)
  let string_of_ss () = Hashtbl.fold (fun k v acc -> (k ^ "," ^ v ^ "\n") ^ acc) h ""

  (* Save the entire contents of the hashtbl to the server *)
  let save_ss_handler =
    handler (fun _ ->
        let ss_string = string_of_ss () in
        (* TODO: replace this with an actual server side save function *)
        let () = ignore @@ %shell_print ss_string in
        Js._true
    )

  let save_button_client () =
    let btn = createButton document in
    btn##onmouseup <- save_ss_handler;
    let body = document##body in
    appendChild body btn

  let escape_cell_handler (td : tableCellElement Js.t) (txt : textAreaElement Js.t) =
    handler (fun (e : keyboardEvent Js.t) -> (
      if e##keyCode = 27 (* Escape Key Code *)
      then (
        store_cell (Js.to_string td##id) (Js.to_string txt##value);
        removeChild td txt;
        td##textContent <- Js.some txt##value
      )
      else ();
      Js._true
    ))

  (* TODO: If the user double clicks on a cell with data in it, retain the existing string *)
  let dbl_click_handler td =
    handler (fun _ -> (
      td##textContent <- Js.null;
      let txt = createTextarea document in
      appendChild td txt;
      td##onkeyup <- escape_cell_handler td txt;
      Js._false
    ))

  (* Build a fresh row as a JS Dom element *)
  let rec fresh_row ?(row = None) ~row_num ~ncols () =
    match row with
    | None ->
      (* Initialize the row and append the row number and an empty td *)
      let init_row = createTr document in
      let rn_td = createTd document in
      rn_td##textContent <- (Js.some @@ Js.string @@ string_of_int row_num);
      appendChild init_row rn_td;
      let new_td = createTd document in
      (* TODO: Add the row_col id to new_td *)
      new_td##id <- Js.string "1_1";
      new_td##ondblclick <- dbl_click_handler new_td;
      appendChild init_row new_td;
      fresh_row ~row:(Some init_row) ~row_num ~ncols ()
    | Some r ->
      if r##cells##length < ncols + 1 (* +1 for row numbering column *)
      then (
        let new_td = createTd document in
        (* TODO: Add the row_col id to new_td *)
        let td_id = (string_of_int row_num) ^ "_" ^ (string_of_int (r##cells##length + 1)) in
        new_td##id <- Js.string td_id;
        new_td##ondblclick <- dbl_click_handler new_td;
        appendChild r new_td;
        fresh_row ~row:(Some r) ~row_num ~ncols ()
      )
      else r

  let rec header_row ?(row = None) ~ncols () =
    let fresh_td col_num =
      let ftd = createTd document in
      ftd##textContent <- (Js.some @@ Js.string @@ string_of_int col_num);
      ftd
    in
    match row with
    | None ->
      (* Initialize the row and append the row number and an empty td *)
      let init_row = createTr document in
      let rn_td = createTd document in
      rn_td##textContent <- (Js.some @@ Js.string "SS");
      appendChild init_row rn_td;
      appendChild init_row (fresh_td 1);
      header_row ~row:(Some init_row) ~ncols ()
    | Some r ->
      if r##cells##length < ncols + 1 (* + 1 for row numbering column *)
      then (
        appendChild r (fresh_td r##cells##length);
        header_row ~row:(Some r) ~ncols ()
      )
      else r

  let rec fresh_table ?(table_body = None) ~nrows ~ncols () =
    let tbdy =
      match table_body with
      | None ->
        let t = createTbody document in
        appendChild t (header_row ~ncols ());
        t
      | Some t -> t
    in
    if tbdy##rows##length < nrows + 1 (* + 1 for headers *)
    then (
      let tr = fresh_row ~row_num:(tbdy##rows##length) ~ncols () in
      appendChild tbdy tr;
      fresh_table ~table_body:(Some tbdy) ~nrows ~ncols ()
    )
    else
      let tbl = createTable document in
      tbl##width <- Js.string "80%";
      tbl##border <- Js.string "1";
      let body = document##body in
      appendChild tbl tbdy;
      appendChild body tbl

}}

let () =
  SS_app.register
    ~service:main_service
    (fun () () ->
     let _ = {unit{fresh_table ~nrows:10 ~ncols:10 ()}} in
     let _ = {unit{save_button_client ()}} in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"SS"
           ~css:[["css";"SS.css"]]
           Html5.F.(body [
               h2 [pcdata "Third Goal: Save and retreive the sheet"]
           ])))
