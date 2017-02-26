(* Functions & data to operate on cell types *)

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

  (* Reload the old spreadsheet data when the page is loaded *)
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
  let num_sheet_rows = 10
  let num_sheet_cols = 10
  let cell_background_color = "#ededed"

  (* Currently selected cell *)
  let (selected_cell : cell option ref) = ref None

  (* indicates status of the shift key & alters other key handlers *)
  let shift_pressed = ref false

  (* Current min and max rows of the sheet *)
  let max_row = ref 0
  let max_col = ref 0

  (* Current area selected with shift *)
  let (shift_area : cell list option ref) = ref None

  let string_of_cell c =
    "\n{\n" ^
    "  row = " ^ (string_of_int c.row) ^ ";\n" ^
    "  col = " ^ (string_of_int c.col) ^ ";\n" ^
    "  id  = " ^ c.id ^ ";\n" ^
    "  txt = " ^ c.txt ^ "\n" ^
    "}"

  let print_shift_area () =
    ignore @@ %shell_print "\nshift_area = ";
    match !shift_area with
    | None -> ignore @@ %shell_print "None";
    | Some sa ->
      let s = List.map (string_of_cell) sa |> List.fold_left (fun s acc -> s ^ acc) "" in
      ignore @@ %shell_print s

  let row_of_id (id : string) =
    String.sub id 0 (String.index id '_') |> int_of_string

  let col_of_id (id : string) =
    let row = string_of_int @@ row_of_id id in
    String.sub id (String.index id '_' + 1) (String.length id - String.length row - 1)
    |> int_of_string

  let key_of_id (id : string) =
    (row_of_id id, col_of_id id)

  (* Given an element id, get the cell *)
  let cell_of_id (id : string) =
    try
    let c = getElementById id in
    Some {row = row_of_id id; col = col_of_id id; id  = id;
          txt = Js.to_string @@ Js.Opt.get (c##textContent) (fun () -> Js.string "")
    }
    with _ -> None (* TODO: Log/Handle specific errors here *)

  (* Get the number of rows in the current shift area *)
  let shift_area_nrows () =
    match !shift_area with
    | None -> 0
    | Some sa ->
      List.map (fun c -> c.row) sa
      |> List.fold_left (fun acc n -> if List.mem n acc then acc else n :: acc) []
      |> List.length

  (* Get the number of columns in the current shift area *)
  let shift_area_ncols () =
    match !shift_area with
    | None -> 0
    | Some sa ->
      List.map (fun c -> c.col) sa
      |> List.fold_left (fun acc n -> if List.mem n acc then acc else n :: acc) []
      |> List.length

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

  let unhighlight_shift_area () =
    match !shift_area with
    | None -> ()
    | Some cl ->
      List.iter (fun (c : cell) ->
          let td = getElementById c.id in
          td##style##backgroundColor <- Js.string cell_background_color
        ) cl

  let shift_release_action td =
    ignore @@ %shell_print "\nshift_release_action called";
    match !selected_cell with
    | None -> ()
    | Some c ->
      let sc = getElementById c.id in
      sc##style##backgroundColor <- Js.string cell_background_color;
      sc##style##border <- Js.string "3px solid black";
      unhighlight_shift_area ();
      shift_area       := None;
      shift_pressed    := false

  (* Get the list of cells that makeup the top row of the the currently selected area *)
  let shift_area_top_row () =
    match !shift_area with
    | None -> []
    | Some sa ->
      let row_nums = List.map (fun c -> c.row) sa in
      let top_row_num = List.fold_left (fun r acc -> if r < acc then r else acc) max_int row_nums in
      List.filter (fun c -> c.row = top_row_num) sa

  (* Get the list of cells that makeup the bottom row of the the currently selected area *)
  let shift_area_bottom_row () =
    match !shift_area with
    | None -> []
    | Some sa ->
      let row_nums = List.map (fun c -> c.row) sa in
      let bot_row_num = List.fold_left (fun r acc -> if r > acc then r else acc) 0 row_nums in
      List.filter (fun c -> c.row = bot_row_num) sa

  (* Get the list of cells that makeup the left col of the the currently selected area *)
  let shift_area_left_col () =
    match !shift_area with
    | None -> []
    | Some sa ->
      let col_nums = List.map (fun c -> c.col) sa in
      let left_col_num =
        List.fold_left (fun c acc -> if c < acc then c else acc) max_int col_nums
      in
      List.filter (fun c -> c.col = left_col_num) sa

  (* Get the list of cells that makeup the right col of the the currently selected area *)
  let shift_area_right_col () =
    match !shift_area with
    | None -> []
    | Some sa ->
      let col_nums = List.map (fun c -> c.col) sa in
      let right_col_num =
        List.fold_left (fun c acc -> if c > acc then c else acc) 0 col_nums
      in
      List.filter (fun c -> c.col = right_col_num) sa

  let rec drop_nones ?(acc = []) (l : 'a option list) =
    match l with
    | [] -> acc
    | (Some x) :: tl -> drop_nones ~acc:(x :: acc) tl
    | None :: tl -> drop_nones ~acc tl

  (* Get the list of cells that makeup the row just above the currently selected area *)
  let row_above_shift_area () =
    match !shift_area  with
    | Some sa ->
      let tr = shift_area_top_row () in
      let top_row_ids = List.map (fun c -> c.id) tr in
      let top_row_keys = List.map (key_of_id) top_row_ids in
      let row_above_ids =
        List.map (fun (r, c) -> (string_of_int (r - 1)) ^ "_" ^ (string_of_int c)) top_row_keys
      in
      let row_above = List.map (cell_of_id) row_above_ids in
      drop_nones row_above
    | None -> []

  (* Get the list of cells that makeup the row just below the currently selected area *)
  let row_below_shift_area () =
    match !shift_area with
    | Some sa ->
      let br = shift_area_bottom_row () in
      let bottom_row_ids = List.map (fun c -> c.id) br in
      let bottom_row_keys = List.map (key_of_id) bottom_row_ids in
      let row_below_ids =
        List.map (fun (r, c) -> (string_of_int (r + 1)) ^ "_" ^ (string_of_int c)) bottom_row_keys
      in
      let row_below = List.map (cell_of_id) row_below_ids in
      drop_nones row_below
    | None -> []

  (* Get the list of cells that makeup the col just to the left of the currently selected area *)
  let col_left_shift_area () =
    match !shift_area with
    | Some sa ->
      let lc = shift_area_left_col () in
      let left_col_ids = List.map (fun c -> c.id) lc in
      let left_col_keys = List.map (key_of_id) left_col_ids in
      List.map (fun (r, c) -> (string_of_int r) ^ "_" ^ (string_of_int (c - 1))) left_col_keys
      |> List.map (cell_of_id) |> drop_nones
    | None -> []

  (* Get the list of cells that makeup the col just to the right of the currently selected area *)
  let col_right_shift_area () =
    match !shift_area with
    | Some sa ->
      let rc = shift_area_right_col () in
      let right_col_ids = List.map (fun c -> c.id) rc in
      let right_col_keys = List.map (key_of_id) right_col_ids in
      List.map (fun (r, c) -> (string_of_int r) ^ "_" ^ (string_of_int (c + 1))) right_col_keys
      |> List.map (cell_of_id) |> drop_nones
    | None -> []

  let update_shift_area direction =
    match !selected_cell, !shift_area, direction with
    | Some sc, Some sa, `Up -> (
        match shift_area_top_row () with
        | [] -> ()
        | hd :: tl ->
            if (hd.row = sc.row && shift_area_nrows () = 1) || hd.row < sc.row
            then shift_area := Some (row_above_shift_area () @ sa)
            else (
              let r = shift_area_bottom_row () |> List.hd |> fun c -> c.row in
              shift_area := Some (List.filter (fun c -> c.row != r) sa)
            )
      )
    | Some sc, Some sa, `Down -> (
        match shift_area_bottom_row () with
        | [] -> ()
        | hd :: tl ->
          if (hd.row = sc.row && shift_area_nrows () = 1) || sc.row < hd.row
          then shift_area := Some (row_below_shift_area () @ sa)
          else (
            let r = shift_area_top_row () |> List.hd |> fun c -> c.row in
            shift_area := Some (List.filter (fun c -> c.row != r) sa)
          )
      )
    | Some sc, Some sa, `Left -> (
        match shift_area_left_col () with
        | [] -> ()
        | hd :: tl ->
          if (hd.col = sc.col && shift_area_ncols () = 1) || hd.col < sc.col
          then shift_area := Some (col_left_shift_area () @ sa)
          else (
            let col_num = shift_area_right_col () |> List.hd |> fun c -> c.col in
            shift_area := Some (List.filter (fun c -> c.col != col_num) sa)
          )
      )
    | Some sc, Some sa, `Right -> (
        match shift_area_right_col () with
        | [] -> ()
        | hd :: tl ->
          if (hd.col = sc.col && shift_area_ncols () = 1) || sc.col < hd.col
          then shift_area := Some (col_right_shift_area () @ sa)
          else (
            let col_num = shift_area_left_col () |> List.hd |> fun c -> c.col in
            shift_area := Some (List.filter (fun c -> c.col != col_num) sa)
          )
      )
    | _, _, _ -> () (* This should not happen, log an error here *)

  let highlight_cells direction =
    match !selected_cell, direction with
    (* If top row num > selected_cell.row then Highlight else Unhighlight *)
    | Some sc, `Up -> (
        match shift_area_top_row () with
        | [] -> ()
        | hd :: tl -> (
            if (hd.row = sc.row && shift_area_nrows () = 1) || hd.row < sc.row
            then
              List.iter (fun c ->
                let td = getElementById c.id in
                td##style##backgroundColor <- Js.string "yellow"
                ) (row_above_shift_area ())
            else
              List.iter (fun c ->
                let td = getElementById c.id in
                td##style##backgroundColor <- Js.string cell_background_color
              ) (shift_area_bottom_row ())
          )
        )
    | Some sc, `Down -> (
        match shift_area_bottom_row () with
        | [] -> ()
        | hd :: tl -> (
          if (hd.row = sc.row && shift_area_nrows () = 1) || sc.row < hd.row
          then
            List.iter (fun c ->
              let td = getElementById c.id in
              td##style##backgroundColor <- Js.string "yellow"
              ) (row_below_shift_area ())
          else
            List.iter (fun c ->
                let td = getElementById c.id in
                td##style##backgroundColor <- Js.string cell_background_color
            ) (shift_area_top_row ())
          )
        )
    | Some sc, `Left -> (
        match shift_area_left_col () with
        | [] -> ()
        | hd :: tl -> (
            if (hd.col = sc.col && shift_area_ncols () = 1) || hd.col < sc.col
            then
              List.iter (fun c ->
                  let td = getElementById c.id in
                  td##style##backgroundColor <- Js.string "yellow"
              ) (col_left_shift_area ())
            else
              List.iter (fun c ->
                  let td = getElementById c.id in
                  td##style##backgroundColor <- Js.string cell_background_color
                ) (shift_area_right_col ())
          )
      )
    | Some sc, `Right -> (
        match shift_area_right_col () with
        | [] -> ()
        | hd :: tl -> (
            if (hd.col = sc.col && shift_area_ncols () = 1) || sc.col < hd.col
            then
              List.iter (fun c ->
                  let td = getElementById c.id in
                  td##style##backgroundColor <- Js.string "yellow"
              ) (col_right_shift_area ())
            else
              List.iter (fun c ->
                  let td = getElementById c.id in
                  td##style##backgroundColor <- Js.string cell_background_color
              ) (shift_area_left_col ())
          )
      )
    | None, _ -> ()

  let shift_and_arrow_handler () =
    handler (fun key_down ->
        match key_down##keyCode with
        | 38 -> highlight_cells `Up; Js._true
        | 40 -> highlight_cells `Down; Js._true
        | 37 -> highlight_cells `Left; Js._true
        | 39 -> highlight_cells `Right; Js._true
        | _ -> Js._true
      )

  (* When the user presses shift                                   *)
  (*  (1) register shift_pressed as true                           *)
  (*  (2) highlight the currently selected cell                    *)
  (*  (3) update shift_area to be the selected cell                *)
  (*  (4) Update last_shift_area to be the currently selected cell *)
  (*  (5) update shift_up, shift_down, shift_left and shift_right  *)
  let shift_pressed_action () =
    match !selected_cell with
    | None -> shift_pressed := true
    | Some c ->
      shift_pressed := true;
      let sc = getElementById c.id in
      sc##style##backgroundColor <- Js.string "yellow";
      shift_area := (
        match !selected_cell with
        | None -> None
        | Some c -> Some [c]
      )

  (* TODO: If the user double clicks on a cell with data in it, retain the existing string *)
  let dbl_click_handler td =
    handler (fun _ ->
      td##textContent <- Js.null;
      let txt = createTextarea document in
      appendChild td txt;
      td##onkeyup <- escape_cell_handler td txt;
      Js._false
    )

  (* Get the cell that is directly above the selected_cell *)
  let up_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
      let row_num = row_of_id sc.id in
      let col = string_of_int @@ col_of_id sc.id in
      if 1 <= row_num - 1
      then cell_of_id (string_of_int (row_num - 1) ^ "_" ^ col)
      else None

  (* Get the cell that is directly below the selected cell *)
  let down_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
      let row_num = row_of_id sc.id in
      let col = string_of_int @@ col_of_id sc.id in
      if row_num + 1 <= !max_row
      then cell_of_id (string_of_int (row_num + 1) ^ "_" ^ col)
      else None

  (* Get the cell that is directly to the left of the selected cell *)
  let left_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
      let row = string_of_int @@ row_of_id sc.id in
      let col_num = col_of_id sc.id in
      if 1 <= col_num - 1
      then cell_of_id (row ^ "_" ^ string_of_int (col_num - 1))
      else None

  (* Get the cell that is directly to the right of the selected cell *)
  let right_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
      let row = string_of_int @@ row_of_id sc.id in
      let col_num = col_of_id sc.id in
      if col_num + 1 <= !max_col
      then cell_of_id (row ^ "_" ^ string_of_int (col_num + 1))
      else None

  (* Actions for an up arrow (other arrow actions follow the same pattern):             *)
  (* If shift is not pressed: ove the selection to the cell above & update selected_cell *)
  (* If shift_pressed: Highlight the cells above shift_area & update shift_area         *)
  let up_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        match up_cell () with
        | None -> ()
        | Some up_c ->
          let sc = getElementById sel_c.id in
          sc##style##border <- Js.string "1px solid black";
          let uc = getElementById up_c.id in
          selected_cell := (Some up_c);
          uc##style##border <- Js.string "3px solid black"
      )
    | Some sel_c, true ->
      highlight_cells `Up;
      update_shift_area `Up
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a down arrow *)
  let down_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        match down_cell () with
        | None -> ()
        | Some down_c ->
          let sc = getElementById sel_c.id in
          sc##style##border <- Js.string "1px solid black";
          let dc = getElementById down_c.id in
          selected_cell := (Some down_c);
          dc##style##border <- Js.string "3px solid black"
      )
    | Some sel_c, true ->
      highlight_cells `Down;
      update_shift_area `Down
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a left arrow *)
  let left_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        match left_cell () with
        | None -> ()
        | Some left_c ->
          let sc = getElementById sel_c.id in
          sc##style##border <- Js.string "1px solid black";
          let lc = getElementById left_c.id in
          selected_cell := (Some left_c);
          lc##style##border <- Js.string "3px solid black"
      )
    | Some sel_c, true ->
      highlight_cells `Left;
      update_shift_area `Left
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a right arrow *)
  let right_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        match right_cell () with
        | None -> ()
        | Some right_c ->
          let sc = getElementById sel_c.id in
          sc##style##border <- Js.string "1px solid black";
          let rc = getElementById right_c.id in
          selected_cell := (Some right_c);
          rc##style##border <- Js.string "3px solid black"
      )
    | Some sel_c, true ->
      highlight_cells `Right;
      update_shift_area `Right
    | None, _ -> () (* Note: This case should never happen *)

  let key_handler =
    handler (fun key_down ->
      match key_down##keyCode with
      | 38 -> up_arrow_action (); Js._true
      | 40 -> down_arrow_action (); Js._true
      | 37 -> left_arrow_action (); Js._true
      | 39 -> right_arrow_action (); Js._true
      | 16 -> shift_pressed_action (); Js._true
      | _ as kc -> ignore @@ %shell_print ("\nkey down = " ^ (string_of_int kc)); Js._true
      )

  let key_release_handler =
    handler (fun key_release ->
        match key_release##keyCode with
        | 16 -> shift_release_action (); Js._true
        | _ as kr -> (); Js._true
      )

  (* On Right Click, bring up a menu. 0 = left click, 2 = right click *)
  let click_handler (td : tableCellElement Js.t) =
    handler (fun (clk : mouseEvent Js.t) ->
      if clk##button = 0
      then (
        match !selected_cell with
        | None -> (
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##border <- Js.string "3px solid black"
          )
        | Some sel_c -> (
            let c = getElementById sel_c.id in
            c##style##border <- Js.string "1px solid black";
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##border <- Js.string "3px solid black"
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
