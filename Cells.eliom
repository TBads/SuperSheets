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

  (* Cells that will be selected when when the user uses arrow keys *)
  let (up_cell : cell option ref)    = ref None
  let (down_cell : cell option ref)  = ref None
  let (left_cell : cell option ref)  = ref None
  let (right_cell : cell option ref) = ref None

  (* Current min and max rows of the sheet *)
  let max_row = ref 0
  let max_col = ref 0

  type shift_select = Highlight of cell list | Unhighlight of cell list

  (* Current area selected with shift *)
  let (shift_area : cell list option ref) = ref None

  (* Last area to be incrementally selected with shift *)
  let (last_shift_area : cell list option ref) = ref None

  (* cells that will be highlighted if shift + and arrow key is used *)
  let (shift_up : shift_select option ref) = ref None
  let (shift_down : shift_select option ref) = ref None
  let (shift_left : shift_select option ref) = ref None
  let (shift_right : shift_select option ref) = ref None

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

  (* for debugging *)
  let string_of_shift dir =
    let shft_cls, d =
      match dir with
      | `Up    -> !shift_up, "up"
      | `Down  -> !shift_down, "down"
      | `Left  -> !shift_left, "left"
      | `Right -> !shift_right, "right"
    in
    match shft_cls with
    | None -> "\nshift_" ^ d ^ " = None"
    | Some (Highlight cl) ->
      let id_list = List.map (fun c -> c.id) cl in
      "\nshift_" ^ d ^ " = Highlight " ^
      (List.fold_left (fun id acc -> id ^ ", " ^ acc) "" id_list)
    | Some (Unhighlight cl) ->
      let id_list = List.map (fun c -> c.id) cl in
      "\nshift_" ^ d ^ " = Unhighlight " ^
      (List.fold_left (fun id acc -> id ^ ", " ^ acc) "" id_list)

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
      shift_up         := None;
      shift_down       := None;
      shift_left       := None;
      shift_right      := None;
      shift_pressed    := false

  let update_shift_up () =
    ignore @@ %shell_print "\n\n\nupdate_shift_up called";
    let () =
      match!selected_cell with
      | Some sc -> ignore @@ %shell_print ("\nselected_cell = " ^ sc.id)
      | None -> ()
    in
    match !shift_area with
    | None -> (); ignore @@ %shell_print "\nupdate_shift_up sees shift_area = None"
    | Some sa ->
      let sel_top_row =
        List.fold_left (fun acc c -> if c.row < acc then c.row else acc) max_int sa
      in
      ignore @@ %shell_print ("\nsel_top_row = " ^ (string_of_int sel_top_row));
      (* TODO Use shift_are_top_row here *)
      let sel_cells_top_row = List.filter (fun c -> c.row = sel_top_row) sa in
      ignore @@ %shell_print "\nsel_cells_top_row =";
      List.iter (fun c -> ignore @@ %shell_print (" " ^ c.id)) sel_cells_top_row;
      if 1 <= sel_top_row - 1
      then (
        ignore @@ %shell_print ("\nList.length sel_cells_top_row = " ^
                                (string_of_int @@ List.length sel_cells_top_row));
        let up_cell_ids =
          List.map
            (fun c -> (string_of_int (c.row - 1)) ^ "_" ^ (string_of_int c.col))
            sel_cells_top_row
        in
        ignore @@ %shell_print "\nup_cell_ids = ";
        List.iter (fun id -> ignore @@ %shell_print (" " ^ id)) up_cell_ids;
        let (up_cells : cell list) =
          List.map (cell_of_id) up_cell_ids
          |> List.filter (fun co -> match co with | None -> false | Some _ -> true)
          |> List.map (fun x -> match x with | Some x -> x | None -> failwith "this cant happen")
        in
        ignore @@ %shell_print "\nup_cells = ";
        List.iter (fun c -> ignore @@ %shell_print (" " ^ c.id)) up_cells;
        let selected_cell_row =
          match !selected_cell with
          | None -> max_int
          | Some sc -> sc.row
        in
        match up_cells, !selected_cell with
        | hd :: tl, Some sc -> (
            ignore @@ %shell_print ("\nup_cells.hd = " ^ hd.id);
            if sel_top_row <= selected_cell_row (* TODO: ??????? *)
            then shift_up := Some (Highlight up_cells)
            else shift_up := Some (Unhighlight up_cells)
          )
        | _ -> ()
      )
      else ((); ignore @@ %shell_print ("\nWHOOPS up"))

  let update_shift_down () =
    ignore @@ %shell_print "\n\n\nupdate_shift_down called";
    let () =
      match!selected_cell with
      | Some sc -> ignore @@ %shell_print ("\nselected_cell = " ^ sc.id)
      | None -> ()
    in
    match !shift_area with
    | None -> (); ignore @@ %shell_print "\nupdate_shift_down sees shift_area = None"
    | Some sa ->
      let max_row = List.fold_left (fun acc c -> if c.row > acc then c.row else acc) 0 sa in
      ignore @@ %shell_print ("\nmax_row = " ^ (string_of_int max_row));
      let bottom_row = List.filter (fun c -> c.row = max_row) sa in
      ignore @@ %shell_print ("\nbottom_row = " ^ (List.hd bottom_row).id);
      if max_row + 1 <= num_sheet_rows
      then (
        ignore @@ %shell_print ("\nList.length bottom_row = " ^
                                (string_of_int @@ List.length bottom_row));
        let down_cell_ids =
          List.map
            (fun (c : cell) -> (string_of_int (c.row + 1)) ^ "_" ^ (string_of_int c.col))
            bottom_row
        in
        List.iter (fun id -> ignore @@ %shell_print ("\ndown_cell_id = " ^ id)) down_cell_ids;
        let (down_cells : cell list) =
          List.map (cell_of_id) down_cell_ids
          |> List.filter (fun co -> match co with | None -> false | Some _ -> true)
          |> List.map (fun x -> match x with | Some x -> x | None -> failwith "this cant happen")
        in
        List.iter (fun c -> ignore @@ %shell_print ("\ndown_cell = " ^ c.id)) down_cells;
        let selected_cell_row =
          match !selected_cell with
          | None -> failwith "need to build a new row"
          | Some sc -> sc.row
        in
        match down_cells, !selected_cell with
        | hd :: tl, Some sc -> (
            if hd.row > sc.row (* TODO: ???????? *)
            then shift_down := Some (Highlight down_cells)
            else shift_down := Some (Unhighlight down_cells)
          );
          update_shift_up ()
        | _ -> ()
      )
      else ((); ignore @@ %shell_print ("\nWHOOPS"))

  let update_shift_left () =
    (*ignore @@ %shell_print "\nupdate_shift_left called";*)
    match !shift_area with
    | None -> ()
    | Some sa ->
      let min_col = List.fold_left (fun acc c -> if c.col < acc then c.row else acc) max_int sa in
      let left_col = List.filter (fun c -> c.col = min_col) sa in
      if 1 <= min_col - 1
      then (
        let left_cell_ids =
          List.map (fun c -> (string_of_int c.row) ^ "_" ^ (string_of_int (c.col - 1))) left_col
        in
        let (left_cells : cell list) =
          List.map (cell_of_id) left_cell_ids
          |> List.filter (fun co -> match co with | None -> false | Some _ -> true)
          |> List.map (fun x -> match x with | Some x -> x | None -> failwith "this cant happen")
        in
        let selected_cell_col =
          match !selected_cell with
          | None -> failwith "This cant happen - update_shift_left\n"
          | Some sc -> sc.col
        in
        if min_col <= selected_cell_col (* TODO: This is not correct and needs to be fixed *)
        then shift_left := Some (Highlight left_cells)
        else shift_left := Some (Unhighlight left_cells)
      )
      else ()

  let update_shift_right () =
    (*ignore @@ %shell_print "\nupdate_shift_right called";*)
    match !shift_area with
    | None -> ()
    | Some sa ->
      let max_col = List.fold_left (fun acc c -> if c.col > acc then c.col else acc) 0 sa in
      let right_col = List.filter (fun c -> c.col = max_col) sa in
      if max_col + 1 <= num_sheet_cols
      then (
        let right_cell_ids =
          List.map (fun c -> (string_of_int c.row) ^ "_" ^ (string_of_int (c.col + 1))) right_col
        in
        let (right_cells : cell list) =
          List.map (cell_of_id) right_cell_ids
          |> List.filter (fun co -> match co with | None -> false | Some _ -> true)
          |> List.map (fun x -> match x with | Some x -> x | None -> failwith "this cant happen")
        in
        let selected_cell_col =
          match !selected_cell with
          | None -> failwith "need to build a new row"
          | Some sc -> sc.col
        in
        if selected_cell_col <= max_col (* TODO: This is not correct and needs to be fixed *)
        then shift_right := Some (Highlight right_cells)
        else shift_right := Some (Unhighlight right_cells)
      )
      else ()

  (* TODO: This should just take a direction like `Up or `Down *)
  (*let update_shift_area (new_shift_cells : shift_select option) =
    match !shift_area, new_shift_cells with
    |_, None -> ()
    | None, Some (Highlight cl) -> shift_area := Some cl
    | None, Some (Unhighlight cl) -> ()
    | Some sa, Some (Highlight cl) ->
      shift_area := Some (
          List.fold_right (fun c acc -> if List.mem c acc then acc else c :: acc) (sa @ cl) []
        )
    | Some sa, Some (Unhighlight cl) ->
      shift_area := Some (List.filter (fun c -> if List.mem c cl then false else true) sa) *)

  (* TODO: Pick back up here and ge the left/right counterparts to the below *)

  (* Get the list of cells that makeup the top row of the the currently selected area *)
  let shift_area_top_row () =
    match !shift_area with
    | None -> None
    | Some sa ->
      let (row_nums : int list) = List.map (fun c -> c.row) sa in
      let (top_row_num : int) =
        List.fold_left (fun r acc -> if r < acc then r else acc) max_int row_nums
      in
      let (top_row : cell list) = List.filter (fun c -> c.row = top_row_num) sa in
      Some top_row

  (* Get the list of cells that makeup the bottom row of the the currently selected area *)
  let shift_area_bottom_row () =
    match !shift_area with
    | None -> None
    | Some sa ->
      let row_nums = List.map (fun c -> c.row) sa in
      let bottom_row_num =
        List.fold_left (fun r acc -> if r > acc then r else acc) 0 row_nums
      in
      let bottom_row = List.filter (fun c -> c.row = bottom_row_num) sa in
      Some bottom_row

  (* Get the list of cells that makeup the left col of the the currently selected area *)
  let shift_area_left_col () =
    match !shift_area with
    | None -> None
    | Some sa ->
      let col_nums = List.map (fun c -> c.col) sa in
      let left_col_num =
        List.fold_left (fun c acc -> if c < acc then c else acc) max_int col_nums
      in
      let left_col = List.filter (fun c -> c.col = left_col_num) sa in
      Some left_col

  (* Get the list of cells that makeup the right col of the the currently selected area *)
  let shift_area_right_col () =
    match !shift_area with
    | None -> None
    | Some sa ->
      let col_nums = List.map (fun c -> c.col) sa in
      let right_col_num =
        List.fold_left (fun c acc -> if c > acc then c else acc) 0 col_nums
      in
      let right_col = List.filter (fun c -> c.col = right_col_num) sa in
      Some right_col

  let rec drop_nones ?(acc = []) (l : 'a option list) =
    match l with
    | [] -> acc
    | (Some x) :: tl -> drop_nones ~acc:(x :: acc) tl
    | None :: tl -> drop_nones ~acc tl

  (* Get the list of cells that makeup the row just above the currently selected area *)
  let row_above_shift_area () =
    match !shift_area, shift_area_top_row () with
    | Some sa, Some tr ->
      let top_row_ids = List.map (fun c -> c.id) tr in
      let top_row_keys = List.map (key_of_id) top_row_ids in
      let row_above_ids =
        List.map (fun (r, c) -> (string_of_int (r-1)) ^ "_" ^ (string_of_int c)) top_row_keys
      in
      let row_above = List.map (cell_of_id) row_above_ids in
      drop_nones row_above
    | _, _ -> []

  (* Get the list of cells that makeup the row just below the currently selected area *)
  let row_below_shift_area () =
    match !shift_area, shift_area_bottom_row () with
    | Some sa, Some br ->
      let bottom_row_ids = List.map (fun c -> c.id) br in
      let bottom_row_keys = List.map (key_of_id) bottom_row_ids in
      let row_below_ids =
        List.map (fun (r, c) -> (string_of_int (r+1)) ^ "_" ^ (string_of_int c)) bottom_row_keys
      in
      let row_below = List.map (cell_of_id) row_below_ids in
      drop_nones row_below
    | _, _ -> []

  (* Get the list of cells that makeup the col just to the left of the currently selected area *)
  let col_left_shift_area () =
    match !shift_area, shift_area_left_col () with
    | Some sa, Some lc ->
      let left_col_ids = List.map (fun c -> c.id) lc in
      let left_col_keys = List.map (key_of_id) left_col_ids in
      List.map (fun (r, c) -> (string_of_int r) ^ "_" ^ (string_of_int (c - 1))) left_col_keys
      |> List.map (cell_of_id) |> drop_nones
    | _, _ -> []

  (* Get the list of cells that makeup the col just to the right of the currently selected area *)
  let col_right_shift_area () =
    match !shift_area, shift_area_right_col () with
    | Some sa, Some lc ->
      let right_col_ids = List.map (fun c -> c.id) lc in
      let right_col_keys = List.map (key_of_id) right_col_ids in
      List.map (fun (r, c) -> (string_of_int r) ^ "_" ^ (string_of_int (c + 1))) right_col_keys
      |> List.map (cell_of_id) |> drop_nones
    | _, _ -> []

  let update_shift_area dir =
    match !shift_area, dir with
    | None, _ -> ()
    | Some sa, `Up -> (
        match shift_area_top_row () with
        | Some r ->
            if (List.hd r).row < 1
            then shift_area := Some (row_above_shift_area () @ sa)
            else ()
        | _ -> ()
      )
    | Some sa, `Down -> (
        match shift_area_bottom_row () with
        | Some r ->
          if (List.hd r).row > !max_row
          then shift_area := Some (row_below_shift_area () @ sa)
          else ()
        | _ -> ()
      )
    | Some sa, `Left -> () (* TODO *)
    | Some sa, `Right -> () (* TODO *)

  let highlight_cells direction =
    let dir, f =
      match direction with
      | `Up    -> !shift_up, update_shift_up
      | `Down  -> !shift_down, update_shift_down
      | `Left  -> !shift_left, update_shift_left
      | `Right -> !shift_right, update_shift_right
    in
    match dir with
    | None -> ()
    | Some (Highlight cl) -> List.iter (fun c ->
        let td = getElementById c.id in
        td##style##backgroundColor <- Js.string "yellow"
      ) cl;
      update_shift_area direction;
      f ()
    | Some (Unhighlight cl) -> List.iter (fun c ->
        let td = getElementById c.id in
        td##style##backgroundColor <- Js.string cell_background_color
      ) cl;
      update_shift_area direction;
      f ()

  let shift_and_arrow_handler () =
    handler (fun key_down ->
        match key_down##keyCode with
        | 38 -> highlight_cells `Up; Js._true
        | 40 -> highlight_cells `Down; Js._true
        | 37 -> highlight_cells `Left; Js._true
        | 39 -> highlight_cells `Right; Js._true
        | _ -> Js._true
      )

  let register_shift_and_arrow () =
    match !shift_up with
    | None -> ()
    | Some ss -> document##body##onkeydown <- shift_and_arrow_handler ()

  let update_shift_cells () =
    match !shift_area with
    | None -> ()
    | Some cl ->
      update_shift_up ();
      update_shift_down ();
      update_shift_left ();
      update_shift_right();
      ignore @@ %shell_print ("\n\n\n\n\n\n\n\n-------------------------------------\nn");
      ignore @@ %shell_print (string_of_shift `Up);
      ignore @@ %shell_print (string_of_shift `Down);
      ignore @@ %shell_print (string_of_shift `Left);
        ignore @@ %shell_print (string_of_shift `Right)

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
      );
      last_shift_area := Some [c];
      update_shift_cells ()

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

  (* Actions for an up arrow (other arrow actions follow the same pattern): *)
  (* If shift_pressed:                                                      *)
  (*   (1) Highlight the cells above shift_area                             *)
  (*   (2) Update shift_area                                                *)
  (*   (3) Update last_shift_area                                           *)
  (*   (4) Update shift_up, shift_down, shift_left and shift_right          *)
  (* If not shift_pressed:                                                  *)
  (*    (1) Move the selection to the cell above                            *)
  let up_arrow_action () =
    match !selected_cell, !up_cell, !shift_pressed with
    | _, None, _ -> ()
    | Some sel_c, Some up_c, false -> (
        let sc = getElementById sel_c.id in
        let uc = getElementById up_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !up_cell;
        uc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string uc##id
      )
    | Some sel_c, Some up_c, true ->
      highlight_cells `Up;
      update_shift_area `Up;
      last_shift_area := shift_area_top_row ();
      update_shift_cells ()
    | None, Some up_id, _ -> () (* Note: This case should never happen *)

  (* Actions for a down arrow *)
  let down_arrow_action () =
    match !selected_cell, !down_cell, !shift_pressed with
    | _, None, _ -> ()
    | Some sel_c, Some down_c, false -> (
        let sc = getElementById sel_c.id in
        let dc = getElementById down_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !down_cell;
        dc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string dc##id
      )
    | Some sel_c, Some down_c, true ->
      highlight_cells `Down;
      update_shift_area `Down;
      last_shift_area := shift_area_bottom_row ();
      update_shift_cells ()
    | None, Some down_id, _ -> () (* Note: This case should never happen *)

  (* Actions for a left arrow *)
  let left_arrow_action () =
    match !selected_cell, !left_cell, !shift_pressed with
    | _, None, _ -> ()
    | Some sel_c, Some left_c, false -> (
        let sc = getElementById sel_c.id in
        let lc = getElementById left_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !left_cell;
        lc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string lc##id
      )
    | Some sel_c, Some left_c, true ->
      highlight_cells `Left;
      update_shift_area `Left;
      last_shift_area := shift_area_left_col ();
      update_shift_cells ()
    | None, Some down_id, _ -> () (* Note: This case should never happen *)

  (* Actions for a right arrow *)
  let right_arrow_action () =
    match !selected_cell, !right_cell, !shift_pressed with
    | _, None, _ -> ()
    | Some sel_c, Some right_c, false -> (
        let sc = getElementById sel_c.id in
        let rc = getElementById right_c.id in
        sc##style##border <- Js.string "1px solid black";
        selected_cell := !right_cell;
        rc##style##border <- Js.string "3px solid black";
        register_key_events @@ Js.to_string rc##id
      )
    | Some sel_c, Some up_c, true ->
      highlight_cells `Right;
      update_shift_area `Right;
      last_shift_area := shift_area_right_col ();
      update_shift_cells ()
    | None, Some right_id, _ -> () (* Note: This case should never happen *)

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
