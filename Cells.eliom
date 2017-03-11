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

  (* NOTE: The order of operations is always: *)
  (*   (1) Update the selected area           *)
  (*   (2) Perform highlighting               *)

  open Dom
  open Dom_html

  type single_cell = {
    row : int;
    col : int;
    id  : string;
    txt : string
  }

  type merged_cell = {
    top_row    : int;
    bottom_row : int;
    left_col   : int;
    right_col  : int;
    id         : string;
    txt        : string
  }

  type cell = SingleCell of single_cell | MergedCell of merged_cell

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
  let (selected_area : cell list option ref) = ref None

  (* Merged cells - keys take the form (row,col), values are the location of the merged cell *)
  (* i.e. the top left corner                                                                *)
  (* Ex. A merged cell covering rows 2&3 and columns 2&3 will have four keys 2_2, 2_3, 3_2   *)
  (*     and 3_3 each with value 2_2                                                         *)
  let merged_h : ((int * int), string) Hashtbl.t = Hashtbl.create 100

  let string_of_cell (c : cell) =
    match c with
    | MergedCell mc -> (
        "\n{" ^
        "  top_row    = " ^ (string_of_int mc.top_row) ^ ";" ^
        "  bottom_row = " ^ (string_of_int mc.bottom_row) ^ ";" ^
        "  left_col   = " ^ (string_of_int mc.left_col) ^ ";" ^
        "  right_col  = " ^ (string_of_int mc.right_col) ^ ";" ^
        "  id         = " ^ mc.id ^ ";" ^
        "  txt        = " ^ mc.txt ^ "" ^
        "}"
      )
    | SingleCell sc ->
      "\n{" ^
      "  row = " ^ (string_of_int sc.row) ^ ";" ^
      "  col = " ^ (string_of_int sc.col) ^ ";" ^
      "  id  = " ^ sc.id ^ ";" ^
      "  txt = " ^ sc.txt ^ "" ^
      "}"

  let string_of_cell_list (cl : cell list option) =
    match cl with
    | None -> "None"
    | Some sa -> List.map (string_of_cell) sa |> List.fold_left (^) ""

  let print_selected_area () =
    ignore @@ %shell_print "\nselected_area = ";
    match !selected_area with
    | None -> ignore @@ %shell_print "None"
    | Some sa ->
      let s = List.map (string_of_cell) sa |> List.fold_left (fun s acc -> s ^ acc) "" in
      ignore @@ %shell_print s

  let print_selected_cell () =
    ignore @@ %shell_print "\nselected_cell = ";
    match !selected_cell with
    | None -> ignore @@ %shell_print "None"
    | Some sc -> ignore @@ %shell_print (string_of_cell sc)

  let row_of_id (id : string) =
    String.sub id 0 (String.index id '_') |> int_of_string

  let col_of_id (id : string) =
    let row = string_of_int @@ row_of_id id in
    String.sub id (String.index id '_' + 1) (String.length id - String.length row - 1)
    |> int_of_string

  let key_of_id (id : string) =
    (row_of_id id, col_of_id id)

  let id_of_key row col =
    (string_of_int row) ^ "_" ^ (string_of_int col)

  let id_of_cell (c : cell) =
    match c with
    | SingleCell sc -> sc.id
    | MergedCell mc -> mc.id

  let txt_of_cell (c : cell) =
    match c with
    | SingleCell sc -> sc.txt
    | MergedCell mc -> mc.txt

  (* Get the ids of single cells that he merged area covers *)
  let single_ids_of_merged_cell (mc : merged_cell) =
    let row_nums =
      Array.create (mc.bottom_row - mc.top_row + 1) 0
      |> (Array.mapi (fun i _ -> i + mc.top_row))
    in
    let col_nums =
      Array.create (mc.right_col - mc.left_col + 1) 0
      |> (Array.mapi (fun i _ -> i + mc.left_col))
    in
    Array.map (fun r -> Array.fold_left (fun acc c -> (r, c) :: acc) [] col_nums) row_nums
    |> Array.to_list
    |> List.flatten

  (* Given an element id, get the cell *)
  let cell_of_id (id : string) =
    (* Change the id if the cell is a merged cell *)
    let real_id =
      match Hashtbl.mem merged_h (key_of_id id) with
      | true  -> Hashtbl.find merged_h (key_of_id id)
      | false -> id
    in
    try
      let c = getElementById real_id in
      let rs =
        c##getAttribute (Js.string "rowspan")
        |> fun o -> Js.Opt.get o (fun () -> Js.string "1")
        |> Js.to_string
        |> int_of_string
      in
      let cs =
        c##getAttribute (Js.string "colspan")
        |> fun o -> Js.Opt.get o (fun () -> Js.string "1")
        |> Js.to_string
        |> int_of_string
      in
      let r_num = row_of_id real_id in
      let c_num = col_of_id real_id in
      match cs * rs with
      | 1 -> Some (SingleCell {
          row = r_num;
          col = c_num;
          id  = real_id;
          txt = Js.to_string @@ Js.Opt.get (c##textContent) (fun () -> Js.string "")
        })
      | _ -> Some (MergedCell {
          top_row    = r_num;
          bottom_row = r_num + rs - 1;
          left_col   = c_num;
          right_col  = c_num + cs - 1;
          id         = real_id;
          txt        = Js.to_string @@ Js.Opt.get (c##textContent) (fun () -> Js.string "")
        })
    with _ -> None (* TODO: Log/Handle specific errors here *)

  (* Get all single_cells from a list of cells *)
  let single_cells (cl : cell list) =
    List.fold_right (fun c acc ->
      match c with
      | SingleCell sc -> sc :: acc
      | MergedCell _ -> acc
      ) cl []

  (* Get all merged_cells from a list of cells *)
  let merged_cells (cl : cell list) =
     List.fold_right (fun c acc ->
       match c with
       | SingleCell _ -> acc
       | MergedCell mc -> mc :: acc
     ) cl []

  (* Get the number of rows in the current shift area *)
  let selected_area_nrows () =
    match !selected_area with
    | None -> 0
    | Some sa ->
      let single_cell_row_nums = List.map (fun c -> c.row) (single_cells sa) in
      let merged_cell_row_nums =
        List.map (single_ids_of_merged_cell) (merged_cells sa)
        |> List.flatten
        |> List.map (fun (r, c) -> r)
      in
      let all_row_nums = single_cell_row_nums @ merged_cell_row_nums in
      List.fold_left (fun acc n -> if List.mem n acc then acc else n :: acc) [] all_row_nums
      |> List.length

  (* Get the number of columns in the current shift area *)
  let selected_area_ncols () =
    match !selected_area with
    | None -> 0
    | Some sa ->
      let single_cell_col_nums = List.map (fun c -> c.col) (single_cells sa) in
      let merged_cell_col_nums =
        List.map (single_ids_of_merged_cell) (merged_cells sa)
        |> List.flatten
        |> List.map (fun (r, c) -> c)
      in
      let all_col_nums = single_cell_col_nums @ merged_cell_col_nums in
      List.fold_left (fun acc n -> if List.mem n acc then acc else n :: acc) [] all_col_nums
      |> List.length

  (* Keys take the form of (row,col), ex. row 1 column 3 has the key "1_3" *)
  let h : ((int * int), string) Hashtbl.t = Hashtbl.create 100

  let store_cell (key : int * int) (value : string) =
    if Hashtbl.mem h key
    then Hashtbl.replace h key value
    else Hashtbl.add h key value

  let store_merged_cell (key : int * int) (value : string) =
    if Hashtbl.mem merged_h key
    then Hashtbl.replace merged_h key value
    else Hashtbl.add merged_h key value

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

  let highlight_cells cl =
    List.iter (fun c ->
        try
          let td = getElementById (id_of_cell c) in
          td##style##backgroundColor <- Js.string "yellow"
        with _ -> () (* TODO: Log error here *)
      ) cl

  let unhighlight_cells cl =
    List.iter (fun c ->
        try
          let td = getElementById (id_of_cell c) in
          td##style##backgroundColor <- Js.string cell_background_color
        with _ -> () (* TODO: Log error here *)
      ) cl

  let highlight_selected_area () =
    match !selected_area with
    | None    -> ()
    | Some sa -> highlight_cells sa

  let unhighlight_selected_area () =
    match !selected_area with
    | None    -> ()
    | Some sa -> unhighlight_cells sa

  let shift_release_action () =
    match !selected_area with
    | None -> ()
    | Some sa -> (
        shift_pressed := false;
        if List.length sa = 1
        then unhighlight_selected_area ()
        else ()
      )

  (* Get the list of cells that makeup the top row of the the currently selected area *)
  let selected_area_top_row () =
    match !selected_area with
    | None -> []
    | Some sa ->
      let single_cell_row_nums = List.map (fun c -> c.row) (single_cells sa) in
      let merged_cell_row_nums =
        List.map (single_ids_of_merged_cell) (merged_cells sa)
        |> List.flatten
        |> List.map (fun (r, c) -> r)
      in
      let all_row_nums = single_cell_row_nums @ merged_cell_row_nums in
      let top_row_num =
        List.fold_left
          (fun r acc -> if r < acc then r else acc)
          max_int
          all_row_nums
      in
      List.filter (fun c ->
          match c with
          | SingleCell sc -> sc.row = top_row_num
          | MergedCell mc -> mc.top_row = top_row_num
      ) sa

  (* Get the list of cells that makeup the bottom row of the the currently selected area *)
  let selected_area_bottom_row () =
    match !selected_area with
    | None -> []
    | Some sa ->
      let single_cell_row_nums = List.map (fun c -> c.row) (single_cells sa) in
      let merged_cell_row_nums =
        List.map (single_ids_of_merged_cell) (merged_cells sa)
        |> List.flatten
        |> List.map (fun (r, c) -> r)
      in
      let all_row_nums = single_cell_row_nums @ merged_cell_row_nums in
      let bot_row_num =
        List.fold_left
          (fun r acc -> if r > acc then r else acc)
          0
          all_row_nums
      in
      List.filter (fun c ->
          match c with
          | SingleCell sc -> sc.row = bot_row_num
          | MergedCell mc -> mc.bottom_row = bot_row_num
      ) sa

  (* Get the list of cells that makeup the left col of the the currently selected area *)
  let selected_area_left_col () =
    match !selected_area with
    | None -> []
    | Some sa ->
      let single_cell_col_nums = List.map (fun c -> c.col) (single_cells sa) in
      let merged_cell_col_nums =
        List.map (single_ids_of_merged_cell) (merged_cells sa)
        |> List.flatten
        |> List.map (fun (r, c) -> c)
      in
      let all_col_nums = single_cell_col_nums @ merged_cell_col_nums in
      let left_col_num =
        List.fold_left
          (fun r acc -> if r < acc then r else acc)
          max_int
          all_col_nums
      in
      List.filter (fun c ->
          match c with
          | SingleCell sc -> sc.col = left_col_num
          | MergedCell mc -> mc.left_col = left_col_num
      ) sa

  (* Get the list of cells that makeup the right col of the the currently selected area *)
  let selected_area_right_col () =
    match !selected_area with
    | None -> []
    | Some sa ->
      let single_cell_col_nums = List.map (fun c -> c.col) (single_cells sa) in
      let merged_cell_col_nums =
        List.map (single_ids_of_merged_cell) (merged_cells sa)
        |> List.flatten
        |> List.map (fun (r, c) -> c)
      in
      let all_col_nums = single_cell_col_nums @ merged_cell_col_nums in
      let right_col_num =
        List.fold_left
          (fun r acc -> if r > acc then r else acc)
          0
          all_col_nums
      in
      List.filter (fun c ->
          match c with
          | SingleCell sc -> sc.col = right_col_num
          | MergedCell mc -> mc.right_col = right_col_num
      ) sa

  let rec drop_nones ?(acc = []) (l : 'a option list) =
    match l with
    | [] -> acc
    | (Some x) :: tl -> drop_nones ~acc:(x :: acc) tl
    | None :: tl -> drop_nones ~acc tl

  (* Get the list of cells that makeup the row just above the currently selected area *)
  let (row_above_selected_area : unit -> cell list) () =
    match !selected_area  with
    | Some sa ->
      let tr = selected_area_top_row () in
      let single_cell_top_row_ids = List.map (fun (c : single_cell) -> c.id) (single_cells tr) in
      let merged_cell_top_row_ids =
        List.map (single_ids_of_merged_cell) (merged_cells tr)
        |> List.flatten
        |> List.map (fun (r, c) -> id_of_key r c)
      in
      let top_row_ids = single_cell_top_row_ids @ merged_cell_top_row_ids in
      let top_row_keys = List.map (key_of_id) top_row_ids in
      let row_above_ids =
        List.map (fun (r, c) -> (string_of_int (r - 1)) ^ "_" ^ (string_of_int c)) top_row_keys
      in
      let (row_above : cell option list) = List.map (cell_of_id) row_above_ids in
      drop_nones row_above
    | None -> []

  (* Get the list of cells that makeup the row just below the currently selected area *)
  let row_below_selected_area () =
    match !selected_area with
    | Some sa ->
      let br = selected_area_bottom_row () in
      let single_cell_bottom_row_ids = List.map (fun (c : single_cell) -> c.id) (single_cells br) in
      let merged_cell_bottom_row_ids =
        List.map (single_ids_of_merged_cell) (merged_cells br)
        |> List.flatten
        |> List.map (fun (r, c) -> id_of_key r c)
      in
      let bottom_row_ids = single_cell_bottom_row_ids @ merged_cell_bottom_row_ids in
      let bottom_row_keys = List.map (key_of_id) bottom_row_ids in
      let row_below_ids =
        List.map (fun (r, c) -> (string_of_int (r + 1)) ^ "_" ^ (string_of_int c)) bottom_row_keys
      in
      let row_below = List.map (cell_of_id) row_below_ids in
      drop_nones row_below
    | None -> []

  (* Get the list of cells that makeup the col just to the left of the currently selected area *)
  let col_left_selected_area () =
    match !selected_area with
    | Some sa ->
      let lc = selected_area_left_col () in
      let single_cell_left_col_ids = List.map (fun (c : single_cell) -> c.id) (single_cells lc) in
      let merged_cell_left_col_ids =
        List.map (single_ids_of_merged_cell) (merged_cells lc)
        |> List.flatten
        |> List.map (fun (r, c) -> id_of_key r c)
      in
      let left_col_ids = single_cell_left_col_ids @ merged_cell_left_col_ids in
      let left_col_keys = List.map (key_of_id) left_col_ids in
      List.map (fun (r, c) -> (string_of_int r) ^ "_" ^ (string_of_int (c - 1))) left_col_keys
      |> List.map (cell_of_id) |> drop_nones
    | None -> []

  (* Get the list of cells that makeup the col just to the right of the currently selected area *)
  let col_right_selected_area () =
    match !selected_area with
    | Some sa ->
      let rc = selected_area_right_col () in
      let single_cell_right_col_ids = List.map (fun (c : single_cell) -> c.id) (single_cells rc) in
      let merged_cell_right_col_ids =
        List.map (single_ids_of_merged_cell) (merged_cells rc)
        |> List.flatten
        |> List.map (fun (r, c) -> id_of_key r c)
      in
      let right_col_ids = single_cell_right_col_ids @ merged_cell_right_col_ids in
      let right_col_keys = List.map (key_of_id) right_col_ids in
      List.map (fun (r, c) -> (string_of_int r) ^ "_" ^ (string_of_int (c + 1))) right_col_keys
      |> List.map (cell_of_id) |> drop_nones
    | None -> []

  (* Get the row from a single_cell or the top row from a merged_cell *)
  let t_row (c : cell) =
    match c with
    | SingleCell sc -> sc.row
    | MergedCell mc -> mc.top_row

  (* Get the row from a single_cell or the bottom row from a merged_cell *)
  let b_row (c : cell) =
    match c with
    | SingleCell sc -> sc.row
    | MergedCell mc -> mc.bottom_row

  (* Get the column from a single_cell or the left column from a merged_cell *)
  let l_col (c : cell) =
    match c with
    | SingleCell sc -> sc.col
    | MergedCell mc -> mc.left_col

  (* Get the col from a single_cell or the right col from a merged_cell *)
  let r_col (c : cell) =
    match c with
    | SingleCell sc -> sc.col
    | MergedCell mc -> mc.right_col

  (* Add missing cells to selected_area after it has been updated                 *)
  (* Ex. If a merged cell covers 2,2 2,3 3,2 & 3,3, then the user selects 4,2,    *)
  (*     presses shift + up, the merged cell and previously selected cell make up *)
  (*     selected_area, but 4,3 will need to be added also                        *)
  let select_missing_cells () =
    (* get the top row, bottom row, left col and right col of selected_area *)
    match !selected_area with
    | None -> ()
    | Some sa -> (
        let tr = List.fold_left (fun acc c -> if t_row c < acc then t_row c else acc) max_int sa in
        let br = List.fold_left (fun acc c -> if b_row c > acc then b_row c else acc) 0 sa in
        let lc = List.fold_left (fun acc c -> if l_col c < acc then l_col c else acc) max_int sa in
        let rc = List.fold_left (fun acc c -> if r_col c > acc then r_col c else acc) 0 sa in
        (* Get a list of all possible single cell ids that are in this area *)
        (* Note: if there is a merged cell in the selected area, then some ids may not exist *)
        let row_nums = Array.create (br - tr + 1) 0 |> (Array.mapi (fun i _ -> i + tr)) in
        let col_nums = Array.create (rc - lc + 1) 0 |> (Array.mapi (fun i _ -> i + lc)) in
        let cell_ids =
          Array.map (fun r -> Array.fold_left (fun acc c -> (r, c) :: acc) [] col_nums) row_nums
          |> Array.to_list
          |> List.flatten
          (* Drop cell ids that are in a larger merged area and are not the id of the merged cell *)
          |> List.filter (fun (r, c) ->
              if Hashtbl.mem merged_h (r, c) && (id_of_key r c) != Hashtbl.find merged_h (r, c)
              then false
              else true
            )
          |> List.map (fun (r, c) -> id_of_key r c)
        in
        (* Add cells to selected_area that are in cell_ids and not in selected_area *)
        let sa_ids = List.map (fun c -> id_of_cell c) sa in
        let non_merged_ids = List.find_all (fun id -> not @@ List.mem id sa_ids) cell_ids in
        let missing_cells = List.map (cell_of_id) non_merged_ids |> drop_nones in
        selected_area := Some (missing_cells @ sa)
      )

  let update_selected_area_up () =
    match !selected_cell, !selected_area with
    | Some sc, Some sa -> (
        match selected_area_top_row () with
        | [] -> ()
        | hd :: tl ->
          if (t_row hd = t_row sc && selected_area_nrows () = 1) || t_row hd < t_row sc
          then selected_area := Some (row_above_selected_area () @ sa)
          else (
            match selected_area_bottom_row () with
            | [] -> ()
            | h :: t -> (
              selected_area := Some (List.filter (fun c -> b_row c != b_row h) sa);
              unhighlight_cells (h :: t)
              )
          );
          select_missing_cells ()
      )
    | _, _ -> ()

  let update_selected_area_down () =
    match !selected_cell, !selected_area with
    | Some sc, Some sa -> (
        match selected_area_bottom_row () with
        | [] -> ()
        | hd :: tl ->
          if (b_row hd = b_row sc && selected_area_nrows () = 1) || b_row sc < b_row hd
          then selected_area := Some (row_below_selected_area () @ sa)
          else (
            match selected_area_top_row () with
            | [] -> ()
            | h :: t as cl -> (
                selected_area := Some (List.filter (fun c -> t_row c != t_row h) sa);
                unhighlight_cells cl
            )
          );
          select_missing_cells ()
      )
    | _, _ -> ()

  let update_selected_area_left () =
    match !selected_cell, !selected_area with
    | Some sc, Some sa -> (
        match selected_area_left_col () with
        | [] -> ()
        | hd :: tl ->
          if (l_col hd = l_col sc && selected_area_ncols () = 1) || l_col hd < l_col sc
          then selected_area := Some (col_left_selected_area () @ sa)
          else (
            match selected_area_right_col () with
            | [] -> ()
            | h :: t as cl -> (
                selected_area := Some (List.filter (fun c -> r_col c != r_col h) sa);
                unhighlight_cells cl
            )
          );
          select_missing_cells ()
      )
    | _,_ -> ()

  let update_selected_area_right () =
    match !selected_cell, !selected_area with
    | Some sc, Some sa -> (
        match selected_area_right_col () with
        | [] -> ()
        | hd :: tl ->
          if (r_col hd = r_col sc && selected_area_ncols () = 1) || r_col sc < r_col hd
          then selected_area := Some (col_right_selected_area () @ sa)
          else (
            match selected_area_left_col () with
            | [] -> ()
            | h :: t as cl -> (
                selected_area := Some (List.filter (fun c -> l_col c != l_col h) sa);
                unhighlight_cells cl
              )
          );
          select_missing_cells ()
      )
    | _, _ -> () (* This should not happen, log an error here *)

  (* Fill in the missing area after cells have been highlighted              *)
  (* Ex. If a merged cell covers 2,2 2,3 3,2 & 3,3, then the user selects 4,2,                 *)
  (*     presses shift + up, the merged cell and previously selected cell will be highlighted, *)
  (*  but 4,3 will need to be highlighted also                                                 *)
  let highlight_missing_area () =
    (* get the top row, bottom row, left col and right col of the highlighted area *)
    match !selected_area with
    | Some sa -> (
        let top_row_num =
          List.fold_left (fun acc c -> if t_row c < acc then t_row c else acc) max_int sa
        in
        let bottom_row_num =
          List.fold_left (fun acc c -> if b_row c > acc then b_row c else acc) 0 sa
        in
        let left_col_num =
          List.fold_left (fun acc c -> if l_col c < acc then l_col c else acc) max_int sa
        in
        let right_col_num =
          List.fold_left (fun acc c -> if r_col c > acc then r_col c else acc) 0 sa
        in
        (* Get a list of all single cells that cover this area *)
        (* Note: if there is a merged cell in the selected area, then some of these may not exist *)
        let row_nums =
        Array.create (bottom_row_num - top_row_num + 1) 0
        |> (Array.mapi (fun i _ -> i + top_row_num))
        in
        let col_nums =
          Array.create (right_col_num - left_col_num + 1) 0
          |> (Array.mapi (fun i _ -> i + left_col_num))
        in
        let cell_ids =
          Array.map (fun r -> Array.fold_left (fun acc c -> (r, c) :: acc) [] col_nums) row_nums
          |> Array.to_list
          |> List.flatten
          |> List.map (fun (r, c) -> id_of_key r c)
        in
        List.iter (fun id ->
          try
            let td = getElementById id in
            td##style##backgroundColor <- Js.string "yellow"
          with _ -> () (* TODO: Log error here *)
        ) cell_ids
      )
    | None -> ()

  let highlight_cells_up () =
    match !selected_cell with
    | Some sc -> (
        match selected_area_top_row () with
        | [] -> ()
        | hd :: tl -> (
            if (t_row hd = t_row sc && selected_area_nrows () = 1) || t_row hd < t_row sc
            then begin
              List.iter (fun c ->
                let td = getElementById (id_of_cell c) in
                td##style##backgroundColor <- Js.string "yellow"
                ) (row_above_selected_area ());
              highlight_missing_area () end
            else
              List.iter (fun c ->
                let td = getElementById (id_of_cell c) in
                td##style##backgroundColor <- Js.string cell_background_color
                ) (selected_area_bottom_row ())
          )
      )
    | None -> ()

  let highlight_cells_down () =
    match !selected_cell with
    | Some sc -> (
        match selected_area_bottom_row () with
        | [] -> ()
        | hd :: tl -> (
          if (b_row hd = b_row sc && selected_area_nrows () = 1) || b_row sc < b_row hd
          then
            List.iter (fun c ->
              let td = getElementById (id_of_cell c) in
              td##style##backgroundColor <- Js.string "yellow"
              ) (row_below_selected_area ())
          else
            List.iter (fun c ->
                let td = getElementById (id_of_cell c) in
                td##style##backgroundColor <- Js.string cell_background_color
            ) (selected_area_top_row ())
          )
      )
    | None -> ()

  let highlight_cells_left () =
    match !selected_cell with
    | Some sc -> (
        match selected_area_left_col () with
        | [] -> ()
        | hd :: tl -> (
            if (l_col hd = l_col sc && selected_area_ncols () = 1) || l_col hd < l_col sc
            then
              List.iter (fun c ->
                  let td = getElementById (id_of_cell c) in
                  td##style##backgroundColor <- Js.string "yellow"
              ) (col_left_selected_area ())
            else
              List.iter (fun c ->
                  let td = getElementById (id_of_cell c) in
                  td##style##backgroundColor <- Js.string cell_background_color
                ) (selected_area_right_col ())
          )
      )
    | None -> ()

  let highlight_cells_right () =
    match !selected_cell with
    | Some sc -> (
        match selected_area_right_col () with
        | [] -> ()
        | hd :: tl -> (
            if (r_col hd = r_col sc && selected_area_ncols () = 1) || r_col sc < r_col hd
            then
              List.iter (fun c ->
                  let td = getElementById (id_of_cell c) in
                  td##style##backgroundColor <- Js.string "yellow"
              ) (col_right_selected_area ())
            else
              List.iter (fun c ->
                  let td = getElementById (id_of_cell c) in
                  td##style##backgroundColor <- Js.string cell_background_color
              ) (selected_area_left_col ())
          )
      )
    | None -> ()

  let shift_and_arrow_handler () =
    handler (fun key_down ->
        match key_down##keyCode with
        | 38 -> highlight_cells_up (); Js._true
        | 40 -> highlight_cells_down (); Js._true
        | 37 -> highlight_cells_left (); Js._true
        | 39 -> highlight_cells_right (); Js._true
        | _ -> Js._true
      )

  (* When the user presses shift                                   *)
  (*  (1) register shift_pressed as true                           *)
  (*  (2) highlight the currently selected cell                    *)
  (*  (3) update selected_area to be the selected cell                *)
  (*  (4) Update last_selected_area to be the currently selected cell *)
  (*  (5) update shift_up, shift_down, shift_left and shift_right  *)
  let shift_pressed_action () =
    match !selected_cell with
    | None -> shift_pressed := true
    | Some c ->
      shift_pressed := true;
      let sc = getElementById (id_of_cell c) in
      sc##style##backgroundColor <- Js.string "yellow";
      selected_area := (
        match !selected_cell with
        | None -> None
        | Some c -> Some [c]
      )

  let dbl_click_handler td =
    handler (fun _ ->
      let existing_text = td##textContent in
      td##textContent <- Js.null;
      let txt = createTextarea document in
      txt##textContent <- existing_text;
      appendChild td txt;
      td##onkeyup <- escape_cell_handler td txt;
      Js._false
    )

  (* Get the cell that is directly above the selected_cell *)
  let up_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
      (*print_selected_cell ();*)
      let row_num = t_row sc in
      let col_num = l_col sc in
      if 1 <= row_num - 1
      then cell_of_id (string_of_int (row_num - 1) ^ "_" ^ string_of_int col_num)
      else None

  (* Get the cell that is directly below the selected cell *)
  let down_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
      let row_num = b_row sc in
      let col_num = l_col sc in
      if row_num + 1 <= !max_row
      then cell_of_id (string_of_int (row_num + 1) ^ "_" ^ string_of_int col_num)
      else None

  (* Get the cell that is directly to the left of the selected cell *)
  let left_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
      let row_num = t_row sc in
      let col_num = l_col sc in
      if 1 <= col_num - 1
      then cell_of_id (string_of_int row_num ^ "_" ^ string_of_int (col_num - 1))
      else None

  (* Get the cell that is directly to the right of the selected cell *)
  let right_cell () =
    match !selected_cell with
    | None -> None
    | Some sc ->
        let row_num = t_row sc in
        let col_num = r_col sc in
        if col_num + 1 <= !max_col
        then cell_of_id (string_of_int row_num ^ "_" ^ string_of_int (col_num + 1))
        else None

  (* Actions for an up arrow (other arrow actions follow the same pattern):             *)
  (* If shift is not pressed: ove the selected_area to the cell above & update selected_cell *)
  (* If shift_pressed: Highlight the cells above selected_area & update selected_area         *)
  let up_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        unhighlight_selected_area ();
        selected_area := None;
        match up_cell () with
        | None -> ()
        | Some up_c ->
            let sc = getElementById @@ id_of_cell sel_c in
            sc##style##border <- Js.string "1px solid black";
            let uc = getElementById @@ id_of_cell up_c in
            selected_cell := (Some up_c);
            uc##style##border <- Js.string "3px solid black";
      )
    | Some sel_c, true -> (
        update_selected_area_up ();
        highlight_selected_area ()
      )
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a down arrow *)
  let down_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        unhighlight_selected_area ();
        selected_area := None;
        match down_cell () with
        | None -> ()
        | Some down_c ->
          let sc = getElementById @@ id_of_cell sel_c in
          sc##style##border <- Js.string "1px solid black";
          let dc = getElementById @@ id_of_cell down_c in
          selected_cell := (Some down_c);
          dc##style##border <- Js.string "3px solid black"
      )
    | Some sel_c, true -> (
        update_selected_area_down ();
        highlight_selected_area ()
      )
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a left arrow *)
  let left_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        unhighlight_selected_area ();
        selected_area := None;
        match left_cell () with
        | None -> ()
        | Some left_c ->
          let sc = getElementById @@ id_of_cell sel_c in
          sc##style##border <- Js.string "1px solid black";
          let lc = getElementById @@ id_of_cell left_c in
          selected_cell := (Some left_c);
          lc##style##border <- Js.string "3px solid black"
      )
    | Some sel_c, true -> (
        update_selected_area_left ();
        highlight_selected_area ();
      )
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a right arrow *)
  let right_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        unhighlight_selected_area ();
        selected_area := None;
        match right_cell () with
        | None -> ()
        | Some right_c ->
          let sc = getElementById @@ id_of_cell sel_c in
          sc##style##border <- Js.string "1px solid black";
          let rc = getElementById @@ id_of_cell right_c in
          selected_cell := (Some right_c);
          rc##style##border <- Js.string "3px solid black"
      )
    | Some sel_c, true -> (
        update_selected_area_right ();
        highlight_selected_area ()
      )
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
        | _ -> (); Js._true
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
          )
        | Some sel_c -> (
            let c = getElementById @@ id_of_cell sel_c in
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
      init_row##id <- Js.string ("row_" ^ (string_of_int row_num));
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

  let register_merged_cell ~top_row_num ~left_col_num ~width ~height =
    let cell_location = (string_of_int top_row_num) ^ "_" ^ (string_of_int left_col_num) in
    let ids =
      Array.init height (fun _ -> left_col_num)
      |> Array.mapi (fun i c -> (top_row_num + i, c))
      |> Array.map (fun (r, c) -> Array.init width (fun i -> (r, c + i)))
      |> Array.to_list
      |> Array.concat
    in
    Array.map (fun (r, c) -> store_merged_cell (r, c) cell_location) ids

  let merge_selected_area () =
    let top_row_num =
      match selected_area_top_row () with
      | [] -> None
      | hd :: tl -> Some (t_row hd)
    in
    let left_col_num =
      match selected_area_left_col () with
      | [] -> None
      | hd :: tl -> Some (l_col hd)
    in
    let width = selected_area_nrows () in
    let height = selected_area_ncols () in
    match top_row_num, left_col_num, !selected_area with
    | Some trn, Some lcn, Some sa ->
      List.iter (fun c ->
          if t_row c != trn || l_col c != lcn
          then (
            let tr = getElementById ("row_" ^ (string_of_int (t_row c))) in
            let td = getElementById @@ id_of_cell c in
            removeChild tr td
          )
          else (
            let tr = getElementById ("row_" ^ (string_of_int @@ t_row c)) in
            let old_td = getElementById @@ id_of_cell c in
            let new_td = createTd document in
            new_td##rowSpan <- width;
            new_td##colSpan <- height;
            new_td##style##backgroundColor <- Js.string cell_background_color;
            new_td##onmousedown <- click_handler new_td;
            new_td##id <- old_td##id;
            replaceChild tr new_td old_td;
            selected_cell := None;
            (* Register the merged cell in h *)
            (* All rows and cols the area covers will be a unique key *)
            register_merged_cell ~top_row_num:trn ~left_col_num:lcn ~width ~height;
            match cell_of_id ((string_of_int trn) ^ "_" ^ (string_of_int lcn)) with
            | None -> ()
            | Some c -> selected_area := Some [c]
          )
        ) sa
    | _, _, _ -> ()

  let merge_area_button () =
    let btn = createButton document in
    btn##textContent <- Js.some @@ Js.string "Merge Cells";
    btn##onmouseup <- handler (fun _ -> merge_selected_area (); Js._true);
    let body = document##body in
    appendChild body btn

}}
