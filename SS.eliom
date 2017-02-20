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

  (* Print a string to the server side shell -- for testing *)
  let shell_print = server_function Json.t<string> Lwt_io.print

  let store_to_disc s =
    let open Lwt_unix in
    lwt fd =
      Lwt_unix.openfile
        "Sheets/TestUser1/TestSheet.ss"
        [O_WRONLY; O_APPEND; O_CREAT; O_TRUNC]
        0o640
    in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    try_lwt
      Lwt_io.write oc s >>
      Lwt_io.close oc
    with _ -> Lwt_io.close oc

    let store_to_disc' = server_function Json.t<string> store_to_disc

  (* TODO: Reload the old spreadsheet data when the page is loaded *)
  let load_from_disc () =
    let open Lwt_unix in
    lwt fd = Lwt_unix.openfile "Sheets/TestUser1/TestSheet.ss" [O_RDONLY] 0o640 in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    try_lwt
      lwt contents = Lwt_io.read ic in
      Lwt_io.close ic >>
      Lwt.return contents
    with _ -> Lwt_io.close ic >> Lwt.return ""

  let load_from_disc' = server_function Json.t<unit> load_from_disc

}}

{client{

  open Dom
  open Dom_html

  type cell = {
    row : int;
    col : int;
    id  : string;
    txt : string
  }

  (* Parameters *)
  let cell_background_color = "#ededed"

  (* Currently selected cell *)
  let (selected_cell : cell option ref) = ref None

  (* Cells that will be selected when when the user uses arrow keys *)
  let (up_cell : cell option ref)    = ref None
  let (down_cell : cell option ref)  = ref None
  let (left_cell : cell option ref)  = ref None
  let (right_cell : cell option ref) = ref None

  (* Current min and max rows of the sheet *)
  let max_row = ref 0
  let max_col = ref 0

  (* Starting position when the shift key is pressed *)
  let (shift_start : cell option ref) = ref None

  (* Current area selected with shift *)
  let (shift_area : cell list option ref) = ref None

  (* cells that will be highlighted if shift + and arrow key is used *)
  let (shift_up : cell list option ref) = ref None
  let (shift_down : cell list option ref) = ref None
  let (shift_left : cell list option ref) = ref None
  let (shift_right : cell list option ref) = ref None

  (* Keys take the form of row_col, ex. row 1 column 3 has the key "1_3" *)
  let h : ((int * int), string) Hashtbl.t = Hashtbl.create 100

  let store_cell (key : int * int) (value : string) =
    if Hashtbl.mem h key
    then Hashtbl.replace h key value
    else Hashtbl.add h key value

  let get_cell key = Hashtbl.find h key

  let json_string_of_ss () =
    let json_content =
      Hashtbl.fold (fun k v acc ->
          acc ^ "\"" ^ ((string_of_int @@ fst k) ^ "_" ^ (string_of_int @@ snd k)) ^
          "\":" ^ "\"" ^ v ^ "\","
        ) h "{"
      |> fun json_string -> String.sub json_string 0 (String.length json_string - 1)
    in
    json_content ^ "}"

  let row_of_id (id : string) =
    String.sub id 0 (String.index id '_') |> int_of_string

  let col_of_id (id : string) =
    let row = string_of_int @@ row_of_id id in
    String.sub id (String.index id '_' + 1) (String.length id - String.length row - 1)
    |> int_of_string

  let key_of_id (id : string) =
    (row_of_id id, col_of_id id)

  (* Parse a spreadsheet string into a (key, value) list *)
  let parse_ss_string (ss_string : string) =
    let json = Yojson.Basic.from_string ss_string in
    let keys = Yojson.Basic.Util.keys json in
    let values = Yojson.Basic.Util.values json in
    List.map2 (fun k v -> match v with | `String s -> [k; s] | _ -> []) keys values
    |> List.filter (fun l -> List.length l > 0)
    |> List.map (fun sl -> ((row_of_id @@ List.hd sl, col_of_id @@ List.hd sl), List.nth sl 1))

  (* Clear all data out of h, and replace it with the new input [key, value] list *)
  let replace_h (kv_list :  ((int * int) * string) list) =
    Hashtbl.clear h;
    List.iter (fun (kv : (int * int) * string) -> store_cell (fst kv) (snd kv)) kv_list

  let update_td (rc : int * int) txt =
    let id = (string_of_int @@ fst rc) ^ "_" ^ (string_of_int @@ snd rc) in
    try
      let td = getElementById id in
      td##textContent <- (Js.some @@ Js.string txt)
    with _ -> ()

  (* Update the DOM with spreadsheet data loaded from disc *)
  let load_and_update () =
    lwt ss_string = %load_from_disc' () in
    let (kv_list : ((int * int) * string) list) = parse_ss_string ss_string in
    replace_h kv_list;
    Hashtbl.iter (fun (k : int * int) (v : string) -> update_td k v) h;
    Lwt.return_unit

  (* Save the entire contents of the hashtbl to the server *)
  let save_ss_handler =
    handler (fun _ ->
        let ss_string = json_string_of_ss () in
        let () = ignore @@ %store_to_disc' ss_string in
        Js._true
    )

  (* Load the entire contents of the spreadsheet from the server *)
  let load_ss_handler =
    handler (fun _ ->
        let () = ignore @@ load_and_update () in
        Js._true
    )

  let load_button () =
    let btn = createButton document in
    btn##textContent <- Js.some @@ Js.string "Load";
    btn##onmouseup <- load_ss_handler;
    let body = document##body in
    appendChild body btn

  let save_button () =
    let btn = createButton document in
    btn##textContent <- Js.some @@ Js.string "Save";
    btn##onmouseup <- save_ss_handler;
    let body = document##body in
    appendChild body btn

  let escape_cell_handler (td : tableCellElement Js.t) (txt : textAreaElement Js.t) =
    handler (fun (e : keyboardEvent Js.t) ->
      if e##keyCode = 27 (* Escape Key Code *)
      then (
        store_cell (key_of_id @@ Js.to_string td##id) (Js.to_string txt##value);
        removeChild td txt;
        td##textContent <- Js.some txt##value
      )
      else ();
      Js._true
    )

  let shift_release_action td =
    ignore @@ %shell_print "\nshift_release_action called";
    match !selected_cell with
    | None -> ()
    | Some c ->
      let sc = getElementById c.id in
      shift_start := None;
      sc##style##backgroundColor <- Js.string cell_background_color

  let shift_surrounding_cells id = ()


  let register_shift_events id =
    (*let up, down, left, right = shift_surrounding_cells id in*)
    ()

  (* Given an element id, get the cell *)
  let cell_of_id (id : string) =
    try
    let c = getElementById id in
    Some {
      row = row_of_id id;
      col = col_of_id id;
      id  = id;
      txt = Js.to_string @@ Js.Opt.get (c##textContent) (fun () -> Js.string "")
    }
    with _ -> None (* TODO: Log/Handle specific errors here *)

  let shift_press_action () =
    match !selected_cell with
    | None -> ()
    | Some c ->
      let sc = getElementById c.id in
      shift_start := cell_of_id @@ Js.to_string sc##id;
      sc##style##backgroundColor <- Js.string "yellow"
      (* TODO: Register the *)

  (* TODO: If the user double clicks on a cell with data in it, retain the existing string *)
  let dbl_click_handler td =
    handler (fun _ ->
      td##textContent <- Js.null;
      let txt = createTextarea document in
      appendChild td txt;
      td##onkeyup <- escape_cell_handler td txt;
      Js._false
    )

  (* Return the (up, down, left, right) cell ids *)
  let surrounding_cells (id : string) =
    let row_i = row_of_id id in
    let row = string_of_int row_i in
    let col_i = col_of_id id in
    let col = string_of_int col_i in
    let up =
      if 1 <= row_i - 1
      then cell_of_id (string_of_int (row_i - 1) ^ "_" ^ col)
      else None
    in
    let down =
      if row_i + 1 <= !max_row
      then cell_of_id (string_of_int (row_i + 1) ^ "_" ^ col)
      else None
    in
    let left =
      if 1 <= col_i - 1
      then cell_of_id (row ^ "_" ^ string_of_int (col_i - 1))
      else None in
    let right =
      if col_i + 1 <= !max_col
      then cell_of_id (row ^ "_" ^ string_of_int (col_i + 1))
      else None
    in
    (up, down, left, right)

  let register_key_events (id : string) =
    let up, down, left, right = surrounding_cells id in
     up_cell := up;
     down_cell := down;
     left_cell := left;
     right_cell := right

  (* Actions for an up arrow *)
  let up_arrow_action () =
    match !selected_cell, !up_cell with
    | None, None -> ()
    | _, None -> ()
    | Some sel_c, Some up_c -> (
        let sc = getElementById sel_c.id in
        let uc = getElementById up_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !up_cell;
        uc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string uc##id
      )
    | None, Some up_id -> () (* Note: This case should never happen *)

  (* Actions for a down arrow *)
  let down_arrow_action () =
    match !selected_cell, !down_cell with
    | None, None -> ()
    | _, None -> ()
    | Some sel_c, Some down_c -> (
        let sc = getElementById sel_c.id in
        let dc = getElementById down_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !down_cell;
        dc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string dc##id
      )
    | None, Some down_id -> () (* Note: This case should never happen *)

  (* Actions for a left arrow *)
  let left_arrow_action () =
    match !selected_cell, !left_cell with
    | None, None -> ()
    | _, None -> ()
    | Some sel_c, Some left_c -> (
        let sc = getElementById sel_c.id in
        let lc = getElementById left_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !left_cell;
        lc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string lc##id
      )
    | None, Some down_id -> () (* Note: This case should never happen *)

  (* Actions for a right arrow *)
  let right_arrow_action () =
    match !selected_cell, !right_cell with
    | None, None -> ()
    | _, None -> ()
    | Some sel_c, Some right_c -> (
        let sc = getElementById sel_c.id in
        let rc = getElementById right_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !right_cell;
        rc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string rc##id
      )
    | None, Some right_id -> () (* Note: This case should never happen *)

  (* Note: up = 38, down = 40, left = 37, right = 39 *)
  let key_handler =
    handler (fun key_press ->
      match key_press##keyCode with
      | 38 -> up_arrow_action (); Js._true
      | 40 -> down_arrow_action (); Js._true
      | 37 -> left_arrow_action (); Js._true
      | 39 -> right_arrow_action (); Js._true
      | 16 -> shift_press_action (); Js._true
      | _ as kc -> ignore @@ %shell_print ("\nkey pressed = " ^ (string_of_int kc)); Js._true
      )

  let key_release_handler =
    handler (fun key_release ->
        match key_release##keyCode with
        | 16 -> shift_release_action (); Js._true
        | _ as kr -> ignore @@ %shell_print ("\nkey released = " ^ (string_of_int kr)); Js._true
      )

  (* On Right Click, bring up a menu. 0 = left click, 2 = right click *)
  let click_handler (td : tableCellElement Js.t) =
    handler (fun (clk : mouseEvent Js.t) ->
      if clk##button = 0
      then (
        match !selected_cell with
        | None -> (
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##border <- Js.string "3px solid black";
            register_key_events @@ Js.to_string td##id
          )
        | Some sel_c -> (
            let c = getElementById sel_c.id in
            c##style##border <- Js.string "1px solid black";
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##border <- Js.string "3px solid black";
            register_key_events @@ Js.to_string td##id
          )
      )
      else ();
      Js._true
    )

  (* Create a new & empty cell *)
  let new_cell id =
    let td = createTd document in
    td##style##backgroundColor <- Js.string cell_background_color;
    td##style##width           <- Js.string "70px";
    td##style##minWidth        <- Js.string "70px";
    td##style##maxWidth        <- Js.string "70px";
    td##style##height          <- Js.string "25px";
    td##style##maxHeight       <- Js.string "25px";
    td##style##overflow        <- Js.string "hidden";
    td##id                     <- Js.string id;
    td##ondblclick             <- dbl_click_handler td;
    td##onmousedown            <- click_handler td;
    td

  (* Create a new row number cell *)
  let new_row_number_cell row_num =
    let rn_td = createTd document in
    rn_td##textContent      <- Js.some @@ Js.string @@ string_of_int row_num;
    rn_td##style##textAlign <- Js.string "center";
    rn_td##style##width     <- Js.string "20px";
    rn_td##style##height    <- Js.string "25px";
    rn_td

  (* Create a new column number cell *)
  let new_col_number_cell col_num =
    let ftd = createTd document in
    ftd##textContent      <- (Js.some @@ Js.string @@ string_of_int col_num);
    ftd##style##textAlign <- Js.string "center";
    ftd##style##width     <- Js.string "70px";
    ftd##style##height    <- Js.string "25px";
    ftd

  (* Build a fresh row as a JS Dom element *)
  let rec fresh_row ?(row = None) ~row_num ~ncols () =
    match row with
    | None ->
      (* Initialize the row and append the row number and an empty td *)
      let init_row = createTr document in
      let rn_td = new_row_number_cell row_num in
      appendChild init_row rn_td;
      let new_td = new_cell ((string_of_int row_num) ^ "_1") in
      appendChild init_row new_td;
      fresh_row ~row:(Some init_row) ~row_num ~ncols ()
    | Some r ->
      if r##cells##length < ncols + 1 (* +1 for row numbering column *)
      then (
        let td_id = (string_of_int row_num) ^ "_" ^ (string_of_int (r##cells##length)) in
        let new_td = new_cell td_id in
        appendChild r new_td;
        fresh_row ~row:(Some r) ~row_num ~ncols ()
      )
      else r

  let rec header_row ?(row = None) ~ncols () =
    match row with
    | None ->
      (* Initialize the row and append the row number and an empty td *)
      let init_row = createTr document in
      let rn_td = createTd document in
      rn_td##textContent <- (Js.some @@ Js.string "SS");
      rn_td##style##textAlign <- Js.string "center";
      appendChild init_row rn_td;
      appendChild init_row (new_col_number_cell 1);
      header_row ~row:(Some init_row) ~ncols ()
    | Some r ->
      if r##cells##length < ncols + 1 (* + 1 for row numbering column *)
      then (
        appendChild r (new_col_number_cell r##cells##length);
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
      max_row := nrows;
      max_col := ncols;
      let tbl = createTable document in
      (*tbl##width <- Js.string "80%";*)
      tbl##border <- Js.string "1";
      tbl##id <- Js.string "main_table";
      tbl##style##borderCollapse <- Js.string "collapse";
      let body = document##body in
      body##onkeydown <- key_handler;
      body##onkeyup <- key_release_handler;
      appendChild tbl tbdy;
      appendChild body tbl

}}

let () =
  SS_app.register
    ~service:main_service
    (fun () () ->
     let _ = {unit{fresh_table ~nrows:10 ~ncols:10 ()}} in
     let _ = {unit{save_button ()}} in
     let _ = {unit{load_button ()}} in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"SS"
           ~css:[["css";"SS.css"]]
           Html5.F.(body [
           ])))
