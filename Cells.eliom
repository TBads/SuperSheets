(* TODO: Need to escape double quotes when stings are saved!  *)
(* TODO: Add a button to add more rows & columns to the sheet *)
(* TODO: Need merge to un-merge already merged cells          *)

{server{

  (* Print a string to the server side shell -- for testing *)
  let shell_print = server_function Json.t<string> Lwt_io.print

  (* TODO: need to be able to retreive merged cells and render them as merged. *)
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
  open Formulas
  open Types

  (* Parameters *)
  let num_sheet_rows = 10
  let num_sheet_cols = 10
  let cell_background_color = "white"

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
    | SingleCell sc ->
      "\n{" ^
      "\n  row    = " ^ (string_of_int sc.row) ^ ";" ^
      "\n  col    = " ^ (string_of_int sc.col) ^ ";" ^
      "\n  id     = " ^ sc.id ^ ";" ^
      "\n  txt_in = " ^ sc.txt_in ^ ";" ^
      "\n  txt    = " ^ sc.txt ^ ";" ^
      "\n  color  = " ^ sc.color ^ ";" ^
      "\n  border_top = " ^ sc.border_top ^ ";" ^
      "\n  border_bottom = " ^ sc.border_bottom ^ ";" ^
      "\n  border_left = " ^ sc.border_left ^ ";" ^
      "\n  border_right = " ^ sc.border_right ^ ";" ^
      "\n  text_align = " ^ sc.text_align ^ ";" ^
      "\n  font_weight = " ^ sc.font_weight ^ ";" ^
      "\n  font_style = " ^ sc.font_style ^
      "\n}"
    | MergedCell mc -> (
        "\n{" ^
        "\n  top_row     = " ^ (string_of_int mc.top_row) ^ ";" ^
        "\n  bottom_row  = " ^ (string_of_int mc.bottom_row) ^ ";" ^
        "\n  left_col    = " ^ (string_of_int mc.left_col) ^ ";" ^
        "\n  right_col   = " ^ (string_of_int mc.right_col) ^ ";" ^
        "\n  id          = " ^ mc.id ^ ";" ^
        "\n  txt_in      = " ^ mc.txt_in ^ ";" ^
        "\n  txt         = " ^ mc.txt ^ ";" ^
        "\n  color       = " ^ mc.color ^ ";" ^
        "\n  border_top  = " ^ mc.border_top ^ ";" ^
        "\n  border_bottom  = " ^ mc.border_bottom ^ ";" ^
        "\n  border_left  = " ^ mc.border_left ^ ";" ^
        "\n  border_right  = " ^ mc.border_right ^ ";" ^
        "\n  text_align = " ^ mc.text_align ^ ";" ^
        "\n  font_weight = " ^ mc.font_weight ^ ";" ^
        "\n  font_style = " ^ mc.font_style ^
        "\n}"
      )

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

  let color_of_cell (c : cell) =
    match c with
    | SingleCell sc -> sc.color
    | MergedCell mc -> mc.color

  let key_of_cell (c : cell) =
    key_of_id @@ id_of_cell c

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

  (* Given an element id, get the cell FROM THE DOM *)
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
          row           = r_num;
          col           = c_num;
          id            = real_id;
          txt_in        = ""; (* TODO *)
          txt           = Js.to_string @@ Js.Opt.get (c##textContent) (fun () -> Js.string "");
          color         = Js.to_string (c##style##backgroundColor);
          border_top    = Js.to_string (c##style##borderTop);
          border_bottom = Js.to_string (c##style##borderBottom);
          border_left   = Js.to_string (c##style##borderLeft);
          border_right  = Js.to_string (c##style##borderRight);
          text_align    = Js.to_string (c##style##textAlign);
          font_weight   = Js.to_string (c##style##fontWeight);
          font_style    = Js.to_string (c##style##fontStyle)
        })
      | _ -> Some (MergedCell {
          top_row       = r_num;
          bottom_row    = r_num + rs - 1;
          left_col      = c_num;
          right_col     = c_num + cs - 1;
          id            = real_id;
          txt_in        = ""; (* TODO *)
          txt           = Js.to_string @@ Js.Opt.get (c##textContent) (fun () -> Js.string "");
          color         = Js.to_string (c##style##backgroundColor);
          border_top    = Js.to_string (c##style##borderTop);
          border_bottom = Js.to_string (c##style##borderBottom);
          border_left   = Js.to_string (c##style##borderLeft);
          border_right  = Js.to_string (c##style##borderRight);
          text_align    = Js.to_string (c##style##textAlign);
          font_weight   = Js.to_string (c##style##fontWeight);
          font_style    = Js.to_string (c##style##fontStyle)
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
  (*let h : ((int * int), cell) Hashtbl.t = Hashtbl.create 100*)

  let txt_in_of_id (id : Js.js_string Js.t) =
    ignore @@ %shell_print "\n\ntxt_in_of_id:";
    let id' = Js.to_string id in
    ignore @@ %shell_print ("\nid' = " ^ id');
    try
      match Hashtbl.find h (key_of_id id') with
      | SingleCell sc -> sc.txt_in
      | MergedCell mc -> mc.txt_in
    with
      not_found -> (ignore @@ %shell_print "\nnot_found"; "")

  let store_cell (key : int * int) (value : cell) =
    if Hashtbl.mem h key
    then Hashtbl.replace h key value
    else Hashtbl.add h key value

  let store_merged_cell (key : int * int) (value : string) =
    if Hashtbl.mem merged_h key
    then Hashtbl.replace merged_h key value
    else Hashtbl.add merged_h key value

  let register_merged_cell ~top_row_num ~left_col_num ~width ~height =
    let cell_location = (string_of_int top_row_num) ^ "_" ^ (string_of_int left_col_num) in
    let ids =
      Array.init height (fun _ -> left_col_num)
      |> Array.mapi (fun i c -> (top_row_num + i, c))
      |> Array.map (fun (r, c) -> Array.init width (fun i -> (r, c + i)))
      |> Array.to_list
      |> Array.concat
    in
    Array.iter (fun (r, c) -> store_merged_cell (r, c) cell_location) ids

  let store_fresh_merged_cell
    ~top_row
    ~left_col
    ~width
    ~height
    ~color
    ~border_top
    ~border_bottom
    ~border_left
    ~border_right
    ~text_align
    ~font_weight
    ~font_style
    txt =
    let bot_row = top_row + height - 1 in
    let right_col = left_col + width - 1 in
    store_cell (top_row, left_col) (MergedCell {
        top_row    = top_row;
        bottom_row = bot_row;
        left_col   = left_col;
        right_col  = right_col;
        id         = id_of_key (top_row) (left_col);
        txt_in     = txt;
        txt        = Formulas.eval_string txt;
        color      = color;
        border_top = border_top;
        border_bottom = border_bottom;
        border_left = border_left;
        border_right = border_right;
        text_align = text_align;
        font_weight = font_weight;
        font_style = font_style
    })

  (* Update the text fields in a cell residing in h *)
  let update_cell_in_h (key : int * int) (txt : string) =
    if Hashtbl.mem h key
    then (
      let old_cell = Hashtbl.find h key in
      let new_cell =
        match old_cell with
        | SingleCell sc -> SingleCell {
            row           = sc.row;
            col           = sc.row;
            id            = sc.id;
            txt_in        = txt;
            txt           = Formulas.eval_string txt;
            color         = sc.color;
            border_top    = sc.border_top;
            border_bottom = sc.border_bottom;
            border_left   = sc.border_left;
            border_right  = sc.border_right;
            text_align    = sc.text_align;
            font_weight   = sc.font_weight;
            font_style    = sc.font_style
          }
        | MergedCell mc -> MergedCell {
            top_row       = mc.top_row;
            bottom_row    = mc.bottom_row;
            left_col      = mc.left_col;
            right_col     = mc.right_col;
            id            = mc.id;
            txt_in        = txt;
            txt           = Formulas.eval_string txt;
            color         = mc.color;
            border_top    = mc.border_top;
            border_bottom = mc.border_bottom;
            border_left   = mc.border_left;
            border_right  = mc.border_right;
            text_align    = mc.text_align;
            font_weight   = mc.font_weight;
            font_style    = mc.font_style
          }
      in
      Hashtbl.replace h key new_cell
    )
    else (
      store_cell key (SingleCell {
          row           = fst key;
          col           = snd key;
          id            = id_of_key (fst key) (snd key);
          txt_in        = txt;
          txt           = Formulas.eval_string txt;
          color         = cell_background_color; (* TODO: should get from DOM *)
          border_top    = "";    (* TODO: should get from DOM *)
          border_bottom = ""; (* TODO: should get from DOM *)
          border_left   = "";   (* TODO: should get from DOM *)
          border_right  = "";  (* TODO: should get from DOM *)
          text_align    = "initial";
          font_weight   = "normal";
          font_style    = "normal"
      })
    )

  (* Update the color field in a cell residing in h *)
  let update_cell_color_in_h (key : int * int) (new_color : string) =
    if Hashtbl.mem h key
    then (
      let old_cell = Hashtbl.find h key in
      let new_cell =
        match old_cell with
        | SingleCell sc -> SingleCell {
            row           = sc.row;
            col           = sc.row;
            id            = sc.id;
            txt_in        = sc.txt_in;
            txt           = sc.txt;
            color         = new_color;
            border_top    = sc.border_top;
            border_bottom = sc.border_bottom;
            border_left   = sc.border_left;
            border_right  = sc.border_right;
            text_align    = sc.text_align;
            font_weight   = sc.font_weight;
            font_style    = sc.font_style
          }
        | MergedCell mc -> MergedCell {
            top_row       = mc.top_row;
            bottom_row    = mc.bottom_row;
            left_col      = mc.left_col;
            right_col     = mc.right_col;
            id            = mc.id;
            txt_in        = mc.txt_in;
            txt           = mc.txt;
            color         = new_color;
            border_top    = mc.border_top;
            border_bottom = mc.border_bottom;
            border_left   = mc.border_left;
            border_right  = mc.border_right;
            text_align    = mc.text_align;
            font_weight   = mc.font_weight;
            font_style    = mc.font_style
          }
      in
      Hashtbl.replace h key new_cell
    )
    else (
      store_cell key (SingleCell {
          row           = fst key;
          col           = snd key;
          id            = id_of_key (fst key) (snd key);
          txt_in        = "";
          txt           = "";
          color         = new_color;
          border_top    = "";
          border_bottom = "";
          border_left   = "";
          border_right  = "";
          text_align    = "initial";
          font_weight   = "normal";
          font_style    = "normal"
      })
    )

  let update_cell_border_in_h ~side (key : int * int) (border : string) =
    if Hashtbl.mem h key
    then (
      let old_cell = Hashtbl.find h key in
      let new_cell =
        match old_cell with
        | SingleCell sc  -> SingleCell {
              row           = sc.row;
              col           = sc.row;
              id            = sc.id;
              txt_in        = sc.txt_in;
              txt           = sc.txt;
              color         = sc.color;
              border_top    = if side = `Top then border else sc.border_top;
              border_bottom = if side = `Bottom then border else sc.border_bottom;
              border_left   = if side = `Left then border else sc.border_left;
              border_right  = if side = `Right then border else sc.border_right;
              text_align    = sc.text_align;
              font_weight   = sc.font_weight;
              font_style    = sc.font_style
            }
        | MergedCell mc -> MergedCell {
              top_row       = mc.top_row;
              bottom_row    = mc.bottom_row;
              left_col      = mc.left_col;
              right_col     = mc.right_col;
              id            = mc.id;
              txt_in        = mc.txt_in;
              txt           = mc.txt;
              color         = mc.color;
              border_top    = if side = `Top then border else mc.border_top;
              border_bottom = if side = `Bottom then border else mc.border_bottom;
              border_left   = if side = `Left then border else mc.border_left;
              border_right  = if side = `Right then border else mc.border_right;
              text_align    = mc.text_align;
              font_weight   = mc.font_weight;
              font_style    = mc.font_style
            }
        in
        Hashtbl.replace h key new_cell
    )
    else (
      store_cell key (SingleCell {
          row           = fst key;
          col           = snd key;
          id            = id_of_key (fst key) (snd key);
          txt_in        = "";
          txt           = "";
          color         = cell_background_color;
          border_top    = if side = `Top then border else "1px solid black";
          border_bottom = if side = `Bottom then border else "";
          border_left   = if side = `Left then border else "1px solid black";
          border_right  = if side = `Right then border else "";
          text_align    = "initial";
          font_weight   = "normal";
          font_style    = "normal"
      })
    )

  let update_cell_text_align_in_h (key : int * int) (text_align : string) =
    if Hashtbl.mem h key
    then (
      let old_cell = Hashtbl.find h key in
      let new_cell =
        match old_cell with
        | SingleCell sc  -> SingleCell {
              row           = sc.row;
              col           = sc.row;
              id            = sc.id;
              txt_in        = sc.txt_in;
              txt           = sc.txt;
              color         = sc.color;
              border_top    = sc.border_top;
              border_bottom = sc.border_bottom;
              border_left   = sc.border_left;
              border_right  = sc.border_right;
              text_align    = text_align;
              font_weight   = sc.font_weight;
              font_style    = sc.font_style
            }
        | MergedCell mc -> MergedCell {
              top_row       = mc.top_row;
              bottom_row    = mc.bottom_row;
              left_col      = mc.left_col;
              right_col     = mc.right_col;
              id            = mc.id;
              txt_in        = mc.txt_in;
              txt           = mc.txt;
              color         = mc.color;
              border_top    = mc.border_top;
              border_bottom = mc.border_bottom;
              border_left   = mc.border_left;
              border_right  = mc.border_right;
              text_align    = text_align;
              font_weight   = mc.font_weight;
              font_style    = mc.font_style
            }
        in
        Hashtbl.replace h key new_cell
    )
    else (
      store_cell key (SingleCell {
          row           = fst key;
          col           = snd key;
          id            = id_of_key (fst key) (snd key);
          txt_in        = "";
          txt           = "";
          color         = cell_background_color;
          border_top    = "1px solid black";
          border_bottom = "";
          border_left   = "1px solid black";
          border_right  = "";
          text_align    = text_align;
          font_weight   = "normal";
          font_style    = "normal"
      })
    )

  let update_cell_font_weight_in_h (key : int * int) (font_weight : string) =
    if Hashtbl.mem h key
    then (
      let old_cell = Hashtbl.find h key in
      let new_cell =
        match old_cell with
        | SingleCell sc  -> SingleCell {
              row           = sc.row;
              col           = sc.row;
              id            = sc.id;
              txt_in        = sc.txt_in;
              txt           = sc.txt;
              color         = sc.color;
              border_top    = sc.border_top;
              border_bottom = sc.border_bottom;
              border_left   = sc.border_left;
              border_right  = sc.border_right;
              text_align    = sc.text_align;
              font_weight   = font_weight;
              font_style    = sc.font_style
            }
        | MergedCell mc -> MergedCell {
              top_row       = mc.top_row;
              bottom_row    = mc.bottom_row;
              left_col      = mc.left_col;
              right_col     = mc.right_col;
              id            = mc.id;
              txt_in        = mc.txt_in;
              txt           = mc.txt;
              color         = mc.color;
              border_top    = mc.border_top;
              border_bottom = mc.border_bottom;
              border_left   = mc.border_left;
              border_right  = mc.border_right;
              text_align    = mc.text_align;
              font_weight   = font_weight;
              font_style    = mc.font_style
            }
        in
        Hashtbl.replace h key new_cell
    )
    else (
      store_cell key (SingleCell {
          row           = fst key;
          col           = snd key;
          id            = id_of_key (fst key) (snd key);
          txt_in        = "";
          txt           = "";
          color         = cell_background_color;
          border_top    = "1px solid black";
          border_bottom = "";
          border_left   = "1px solid black";
          border_right  = "";
          text_align    = "center";
          font_weight   = "normal";
          font_style    = "normal"
      })
    )

  let update_cell_font_style_in_h (key : int * int) (font_style : string) =
    if Hashtbl.mem h key
    then (
      let old_cell = Hashtbl.find h key in
      let new_cell =
        match old_cell with
        | SingleCell sc  -> SingleCell {
              row           = sc.row;
              col           = sc.row;
              id            = sc.id;
              txt_in        = sc.txt_in;
              txt           = sc.txt;
              color         = sc.color;
              border_top    = sc.border_top;
              border_bottom = sc.border_bottom;
              border_left   = sc.border_left;
              border_right  = sc.border_right;
              text_align    = sc.text_align;
              font_weight   = sc.font_weight;
              font_style    = font_style
            }
        | MergedCell mc -> MergedCell {
              top_row       = mc.top_row;
              bottom_row    = mc.bottom_row;
              left_col      = mc.left_col;
              right_col     = mc.right_col;
              id            = mc.id;
              txt_in        = mc.txt_in;
              txt           = mc.txt;
              color         = mc.color;
              border_top    = mc.border_top;
              border_bottom = mc.border_bottom;
              border_left   = mc.border_left;
              border_right  = mc.border_right;
              text_align    = mc.text_align;
              font_weight   = mc.font_weight;
              font_style    = font_style
            }
        in
        Hashtbl.replace h key new_cell
    )
    else (
      store_cell key (SingleCell {
          row           = fst key;
          col           = snd key;
          id            = id_of_key (fst key) (snd key);
          txt_in        = "";
          txt           = "";
          color         = cell_background_color;
          border_top    = "1px solid black";
          border_bottom = "";
          border_left   = "1px solid black";
          border_right  = "";
          text_align    = "center";
          font_weight   = "normal";
          font_style    = "normal"
      })
    )

  let get_cell key = Hashtbl.find h key

  let json_of_cell (c : cell) =
     match c with
     | SingleCell sc -> (
         "\"SingleCell\" : {" ^
           "\"row\" : " ^ (string_of_int sc.row) ^ "," ^
           "\"col\" : " ^ (string_of_int sc.col) ^ "," ^
           "\"id\" : \"" ^ sc.id ^ "\"," ^
           "\"txt_in\" : \"" ^ sc.txt_in ^ "\"," ^
           "\"txt\" : \"" ^ sc.txt ^ "\"," ^
           "\"color\" : \"" ^ sc.color ^ "\"," ^
           "\"border_top\" : \"" ^ sc.border_top ^ "\"," ^
           "\"border_bottom\" : \"" ^ sc.border_bottom ^ "\"," ^
           "\"border_left\" : \"" ^ sc.border_left ^ "\"," ^
           "\"border_right\" : \"" ^ sc.border_right ^ "\"," ^
           "\"text_align\" : \"" ^ sc.text_align ^ "\"," ^
           "\"font_weight\" : \"" ^ sc.font_weight ^ "\"," ^
           "\"font_style\" : \"" ^ sc.font_style ^ "\"" ^
         "}")
     | MergedCell mc ->
         "\"MergedCell\" : {" ^
          "\"top_row\" : " ^ (string_of_int mc.top_row) ^ "," ^
          "\"bottom_row\" : " ^ (string_of_int mc.bottom_row) ^ "," ^
          "\"left_col\" : " ^ (string_of_int mc.left_col) ^ "," ^
          "\"right_col\" : " ^ (string_of_int mc.right_col) ^ "," ^
          "\"id\" : \"" ^ mc.id ^ "\"," ^
          "\"txt_in\" : \"" ^ mc.txt_in ^ "\"," ^
          "\"txt\" : \"" ^ mc.txt ^ "\"," ^
          "\"color\" : \"" ^ mc.color ^ "\"," ^
          "\"border_top\" : \"" ^ mc.border_top ^ "\"," ^
          "\"border_bottom\" : \"" ^ mc.border_bottom ^ "\"," ^
          "\"border_left\" : \"" ^ mc.border_left ^ "\"," ^
          "\"border_right\" : \"" ^ mc.border_right ^ "\"," ^
          "\"text_align\" : \"" ^ mc.text_align ^ "\"," ^
          "\"font_weight\" : \"" ^ mc.font_weight ^ "\"," ^
          "\"font_style\" : \"" ^ mc.font_style ^ "\"" ^
        "}"

  let cell_of_json ((s, j) : string * Yojson.Basic.json) =
    let open Yojson.Basic.Util in
    match s with
    | "SingleCell" -> SingleCell {
        row           = to_int @@ member "row" j;
        col           = to_int @@ member "col" j;
        id            = to_string @@ member "id" j;
        txt_in        = to_string @@ member "txt_in" j;
        txt           = to_string @@ member "txt" j;
        color         = to_string @@ member "color" j;
        border_top    = to_string @@ member "border_top" j;
        border_bottom = to_string @@ member "border_bottom" j;
        border_left   = to_string @@ member "border_left" j;
        border_right  = to_string @@ member "border_right" j;
        text_align    = to_string @@ member "text_align" j;
        font_weight   = to_string @@ member "font_weight" j;
        font_style    = to_string @@ member "font_style" j
      }
    | "MergedCell" -> MergedCell {
        top_row       = to_int @@ member "top_row" j;
        bottom_row    = to_int @@ member "bottom_row" j;
        left_col      = to_int @@ member "left_col" j;
        right_col     = to_int @@ member "right_col" j;
        id            = to_string @@ member "id" j;
        txt_in        = to_string @@ member "txt_in" j;
        txt           = to_string @@ member "txt" j;
        color         = to_string @@ member "color" j;
        border_top    = to_string @@ member "border_top" j;
        border_bottom = to_string @@ member "border_bottom" j;
        border_left   = to_string @@ member "border_left" j;
        border_right  = to_string @@ member "border_right" j;
        text_align    = to_string @@ member "text_align" j;
        font_weight   = to_string @@ member "font_weight" j;
        font_style    = to_string @@ member "font_style" j
      }
    | _ -> (
        ignore @@ %shell_print "Error: Reveived bad json!";
        failwith "Error: Received bad json!"
      )

  let json_string_of_ss () =
    let json_content =
      Hashtbl.fold (fun k v acc -> acc ^ (json_of_cell v) ^ ",") h "{"
      |> fun json_string -> String.sub json_string 0 (String.length json_string - 1)
    in
    json_content ^ "}"

  (* Parse a spreadsheet string into a ((int * int), cell) list *)
  let parse_ss_string (ss_string : string) =
  (*let () =
      Yojson.Basic.from_string ss_string
      |> Yojson.Basic.Util.to_assoc
      |> List.map cell_of_json
      |> List.iter (fun c -> ignore @@ %shell_print @@ string_of_cell c)
    in*)
    let point_1 = Yojson.Basic.from_string ss_string in
    let point_2 = Yojson.Basic.Util.to_assoc point_1 in
    let point_3 = List.map cell_of_json point_2 in
    (*List.iter (fun c -> ignore @@ %shell_print @@ string_of_cell c) point_3;*)
    Yojson.Basic.from_string ss_string
    |> Yojson.Basic.Util.to_assoc
    |> List.map cell_of_json

  (* Clear all data out of h, and replace it with the new input [key, value] list *)
  let replace_h (kv_list :  ((int * int) * cell) list) =
    ignore @@ %shell_print "\n\nreplace_h:\n";
    Hashtbl.clear h;
    List.iter (fun (kv : (int * int) * cell) -> store_cell (fst kv) (snd kv)) kv_list

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

  let cell_list_top_row (cl : cell list) =
    let single_cell_row_nums = List.map (fun c -> c.row) (single_cells cl) in
    let merged_cell_row_nums =
      List.map (single_ids_of_merged_cell) (merged_cells cl)
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
    List.filter (fun c -> t_row c = top_row_num) cl

  let cell_list_bottom_row (cl : cell list) =
    let single_cell_row_nums = List.map (fun c -> c.row) (single_cells cl) in
    let merged_cell_row_nums =
      List.map (single_ids_of_merged_cell) (merged_cells cl)
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
    List.filter (fun c -> b_row c = bot_row_num) cl

  let cell_list_left_col (cl : cell list) =
    let single_cell_col_nums = List.map (fun c -> c.col) (single_cells cl) in
    let merged_cell_col_nums =
      List.map (single_ids_of_merged_cell) (merged_cells cl)
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
    List.filter (fun c -> l_col c = left_col_num) cl

  let cell_list_right_col (cl : cell list) =
    let single_cell_col_nums = List.map (fun c -> c.col) (single_cells cl) in
    let merged_cell_col_nums =
      List.map (single_ids_of_merged_cell) (merged_cells cl)
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
    List.filter (fun c -> r_col c = right_col_num) cl

  let cell_list_nrows (cl : cell list) =
    let top_row =
      List.fold_left (fun acc c -> if t_row c < acc then t_row c else acc) max_int cl
    in
    let bot_row =
      List.fold_left (fun acc c -> if t_row c > acc then t_row c else acc) 0 cl
    in
    bot_row - top_row + 1

  let cell_list_ncols (cl : cell list) =
    let left_col =
      List.fold_left (fun acc c -> if l_col c < acc then l_col c else acc) max_int cl
    in
    let right_col =
      List.fold_left (fun acc c -> if r_col c > acc then r_col c else acc) 0 cl
    in
    right_col - left_col + 1

  let merge_checks (cl : cell list) =
    (* Check 1 - All cells are single cells *)
    let check_1 = List.fold_left (fun acc c ->
        match c with
        | SingleCell _ -> true && acc
        | MergedCell _ -> false && acc
      ) true cl
    in
    (* Check 2 - The number of single cells = nrows * ncols *)
    let nrows = cell_list_nrows cl in
    let ncols = cell_list_ncols cl in
    let check_2 = List.length cl = nrows * ncols in
    (* Check 3 - All rows of cells in selected_area are in top_row & bot_row, same for cols *)
    let top_row_num =
      match cell_list_top_row cl with
      | [] -> -1
      | hd :: tl -> t_row hd
    in
    let bot_row_num =
      match cell_list_bottom_row cl with
      | [] -> -1
      | hd :: tl -> b_row hd
    in
    let left_col_num =
      match cell_list_left_col cl with
      | [] -> -1
      | hd :: tl -> l_col hd
    in
    let right_col_num =
      match cell_list_right_col cl with
      | [] -> -1
      | hd :: tl -> r_col hd
    in
    (* Since all cells are check to be single cells at check 1, just use t_row and l_col *)
    let check_3 =
      List.fold_left (fun acc c ->
          if ((t_row c) >= top_row_num &&
              (t_row c) <= bot_row_num &&
              (l_col c) >= left_col_num &&
              (r_col c) <= right_col_num)
          then true
          else false
        ) true cl
    in
    match check_1, check_2, check_3 with
    | true, true, true -> `Pass
    | false, _, _      -> `Fail "Cannot merge already merged cells!"
    | true, _, _       -> `Fail "Only a rectangular area can be merged"

  let add_to_selected_area (co : cell option) =
    match !selected_area, co with
    | _, None      -> ()
    | None, Some c -> selected_area := Some [c]
    | Some sa, Some c -> selected_area := Some (c :: sa)

  let existing_border_style ?style ~side (td : Dom_html.element Js.t) =
    let f c =
      match c, side with
      | SingleCell sc, `Top    -> sc.border_top
      | SingleCell sc, `Bottom -> sc.border_bottom
      | SingleCell sc, `Left   -> sc.border_left
      | SingleCell sc, `Right  -> sc.border_right
      | MergedCell sc, `Top    -> sc.border_top
      | MergedCell sc, `Bottom -> sc.border_bottom
      | MergedCell sc, `Left   -> sc.border_left
      | MergedCell sc, `Right  -> sc.border_right
    in
    match style with
    | Some s -> s
    | None ->
      let id = key_of_id (Js.to_string td##id) in
      if Hashtbl.mem h id
      then (f @@ Hashtbl.find h id)
      else (
        match side with
        | `Top    -> "1px solid black"
        | `Bottom -> "0px"
        | `Left   -> "1px solid black"
        | `Right  -> "0px"
      )

  (* On Right Click, bring up a menu. 0 = left click, 2 = right click *)
  let click_handler (td : tableCellElement Js.t) =
    handler (fun (clk : mouseEvent Js.t) ->
      if clk##button = 0
      then (
        match !selected_cell, !shift_pressed with
        | None, true -> (
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##borderTop       <- Js.string "3px solid black";
            td##style##borderBottom    <- Js.string "2px solid black";
            td##style##borderLeft      <- Js.string "3px solid black";
            td##style##borderRight     <- Js.string "2px solid black";
            td##style##backgroundColor <- Js.string "rgba(102, 175, 233, 0.5)"
          )
        | None, false -> (
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##borderTop    <- Js.string "3px solid black";
            td##style##borderBottom <- Js.string "2px solid black";
            td##style##borderLeft   <- Js.string "3px solid black";
            td##style##borderRight  <- Js.string "2px solid black"
          )
        | Some sel_c, true -> (
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##borderTop       <- Js.string "3px solid black";
            td##style##borderBottom    <- Js.string "2px solid black";
            td##style##borderLeft      <- Js.string "3px solid black";
            td##style##borderRight     <- Js.string "2px solid black";
            td##style##backgroundColor <- Js.string "rgba(102, 175, 233, 0.5)";
            add_to_selected_area !selected_cell
          )
        | Some sel_c, false -> (
            let c = getElementById @@ id_of_cell sel_c in
            c##style##borderTop    <- Js.string @@ existing_border_style ~side:`Top c;
            c##style##borderBottom <- Js.string @@ existing_border_style ~side:`Bottom c;
            c##style##borderLeft   <- Js.string @@ existing_border_style ~side:`Left c;
            c##style##borderRight  <- Js.string @@ existing_border_style ~side:`Right c;
            selected_cell := cell_of_id (Js.to_string td##id);
            td##style##borderTop    <- Js.string "3px solid black";
            td##style##borderBottom <- Js.string "2px solid black";
            td##style##borderLeft   <- Js.string "3px solid black";
            td##style##borderRight  <- Js.string "2px solid black"
          )
      )
      else ();
      Js._true
    )

  let escape_cell_handler
    (td  : tableCellElement Js.t)
    (txt : inputElement Js.t)
    (div : divElement Js.t) =
      handler (fun (e : keyboardEvent Js.t) ->
        if e##keyCode = 27 (* Escape Key Code *)
        then (
          update_cell_in_h (key_of_id @@ Js.to_string td##id) (Js.to_string txt##value);
          removeChild document##body div;
          td##textContent <-
            Js.to_string txt##value
            |> Formulas.eval_string
            |> Js.string
            |> Js.some; (*txt##value;*)
        )
        else ();
        Js._true
      )

  let formula_bar ~td () =
    let div = createDiv document in
    div##className <- Js.string "input-group";
    let span = createSpan document in
    span##className <- Js.string "input-group-addon";
    span##id <- Js.string "basic-addon1";
    span##innerHTML <- td##id;
    let txt = createInput ~_type:(Js.string "text") document in
    txt##className <- Js.string "form-control";
    txt##style##width <- Js.string "100%";
    txt##placeholder <- Js.string "Type Here...";
    txt##defaultValue <- Js.string (txt_in_of_id td##id);
    appendChild div span;
    appendChild div txt;
    appendChild document##body div;
    txt##focus ();
    txt##onkeyup <- escape_cell_handler td txt div

  let dbl_click_handler (td : tableCellElement Js.t) =
    handler (fun _ ->
      td##textContent <- Js.null;
      formula_bar ~td ();
      Js._false
    )

  (* TODO: Why won't this work? Maybe I need to stop the default action *)
  (*let f2_prevent_default (td : tableCellElement Js.t) =
    handler (fun key_release ->
        match key_release##keyCode with
        | 113 -> (preventDefault key_release; Js._true)
        | _ -> Js._true
      )

  let f2_handler (td : tableCellElement Js.t) =
    ignore @@ %shell_print "\n\nf2_handler called\n\n";
    handler (fun key_release ->
        match key_release##keyCode with
        | 113 -> (
            td##textContent <- Js.null;
            formula_bar ~td ();
            Js._true
          )
        | _ -> Js._true
    )*)

  let merge_area ?(blank_cell = false) (cl : cell list) =
    match merge_checks cl with
    | `Fail msg -> window##alert (Js.string msg)
    | `Pass ->
      let top_row_num =
        match cell_list_top_row cl with
        | [] -> None
        | hd :: tl -> Some (t_row hd)
      in
      let left_col_num =
        match cell_list_left_col cl with
        | [] -> None
        | hd :: tl -> Some (l_col hd)
      in
      let width = cell_list_ncols cl in
      let height = cell_list_nrows cl in
      match top_row_num, left_col_num with
      | Some trn, Some lcn ->
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
              new_td##rowSpan <- height;
              new_td##colSpan <- width;
              new_td##style##backgroundColor <- Js.string cell_background_color;
              new_td##onmousedown <- click_handler new_td;
              (*new_td##onkeyup <- f2_handler new_td;*)
              new_td##ondblclick <- dbl_click_handler new_td;
              new_td##id <- old_td##id;
              if blank_cell
              then
                store_fresh_merged_cell
                  ~top_row:trn
                  ~left_col:lcn
                  ~width
                  ~height
                  ~color:cell_background_color
                  ~border_top:""
                  ~border_bottom:""
                  ~border_left:""
                  ~border_right:""
                  ~text_align:"initial"
                  ~font_weight:"normal" (* TODO: get from DOM? *)
                  ~font_style:"normal" (* TODO: get from DOM? *)
                  ""
              else ();
              replaceChild tr new_td old_td;
              selected_cell := None;
              (* Register the merged cell in h *)
              (* All rows and cols the area covers will be a unique key *)
              register_merged_cell ~top_row_num:trn ~left_col_num:lcn ~width ~height;
              match cell_of_id ((string_of_int trn) ^ "_" ^ (string_of_int lcn)) with
              | None -> ()
              | Some c -> selected_area := Some [c]
            )
          ) cl
      | _, _ -> ()

(* TODO: Right now merged cells are not stored/properly loaded 100% of the time?...*)

  let render_merged_cell (mc : merged_cell) =
    (* If the cell is already rendered, do nothing *)
    match cell_of_id mc.id with
    | None -> ()
    | Some (MergedCell mc) -> ()
    | Some (SingleCell sc) ->
        (* Check if cells needed are actually single cells *)
        let row_nums =
          Array.create (mc.bottom_row - mc.top_row + 1) 0
          |> (Array.mapi (fun i _ -> i + mc.top_row))
        in
        let col_nums =
          Array.create (mc.right_col - mc.left_col + 1) 0
          |> (Array.mapi (fun i _ -> i + mc.left_col))
        in
        let cell_ids =
          Array.map (fun r -> Array.fold_left (fun acc c -> (r, c) :: acc) [] col_nums) row_nums
          |> Array.to_list
          |> List.flatten
          |> List.map (fun (r, c) -> id_of_key r c)
        in
        let all_cells =
          List.map (cell_of_id) cell_ids
          |> List.map (fun co ->
              match co with
              | Some c -> c
              | None -> failwith "ERROR: render_merged_cell" (* TODO: Log Error *)
            )
        in
        let all_single_cells =
          List.fold_left (fun acc id ->
            try
              match cell_of_id id with
              | Some (SingleCell _) -> true && acc
              | _ -> false && acc
            with _ -> false && acc
            ) true cell_ids
        in
        (* If all single sells are availble, then perform the merge process *)
        if all_single_cells
        then (
          match merge_checks all_cells with
          | `Fail msg ->  window##alert (Js.string msg)
          | `Pass -> merge_area all_cells
        )
        else window##alert (Js.string "Not all cells selected are single cells!")

  (* TODO: Update this to account for cell borders, font weight, text-align *)
  let update_td (rc : int * int) c =
    match c with
    | SingleCell sc -> (
        try
          let td = getElementById sc.id in
          td##textContent            <- (Js.some @@ Js.string sc.txt);
          td##style##backgroundColor <- Js.string sc.color;
          td##style##borderTop       <- Js.string sc.border_top;
          td##style##borderLeft      <- Js.string sc.border_left;
          td##style##textAlign       <- Js.string sc.text_align;
          td##style##fontWeight      <- Js.string sc.font_weight;
          td##style##fontStyle       <- Js.string sc.font_style
        with _ -> ()
      )
    | MergedCell mc ->
      try
        render_merged_cell mc;
        let td = getElementById mc.id in
        td##textContent            <- (Js.some @@ Js.string (mc.txt));
        td##style##backgroundColor <- Js.string mc.color;
        td##style##borderTop       <- Js.string mc.border_top;
        td##style##borderLeft      <- Js.string mc.border_left;
        td##style##textAlign       <- Js.string mc.text_align;
        td##style##fontWeight      <- Js.string mc.font_weight;
        td##style##fontStyle       <- Js.string mc.font_style
      with _ -> ()

  (* Update the DOM with spreadsheet data loaded from disc *)
  let load_and_update () =
    lwt ss_string = %load_from_disc' () in
    let (kv_list : ((int * int) * cell) list) =
      parse_ss_string ss_string |> List.map (fun c -> ((key_of_cell c), c))
    in
    replace_h kv_list;
    Hashtbl.iter (fun (k : int * int) (v : cell) -> update_td k v) h;
    Lwt.return_unit

  (* Save the entire contents of the hashtbl to the server *)
  (*let save_ss_handler =
    handler (fun _ ->
        let ss_string = json_string_of_ss () in
        let () = ignore @@ %store_to_disc' ss_string in
        Js._true
    )
  *)

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

  let cancel_save_ss_handler (modal : divElement Js.t) =
    handler (fun _ ->
        modal##style##display <- Js.string "none";
        Js._true
    )

  let show_save_confirm_modal msg =
    let div = createDiv document in
    let msg_div = createDiv document in
    div##id <- Js.string "save_confirm_modal";
    div##className <- Js.string "modal";

    msg_div##id <- Js.string "save_confirm_modal_msg";
    msg_div##textContent <- Js.some @@ Js.string msg;

    let close_btn = createButton document in
    close_btn##className <- Js.string "btn btn-success";
    close_btn##id <- Js.string "CloseSaveConfirmBtn";
    close_btn##textContent <- Js.some @@ Js.string "OK";
    close_btn##onmouseup <- handler (fun _ -> div##style##display <- Js.string "none"; Js._true);
    appendChild div msg_div;
    appendChild div close_btn;
    appendChild document##body div

  let complete_save_ss_handler (modal : divElement Js.t) username sheet_name_input sheet_data =
    handler (fun _ ->
        Lwt.async (
          fun () -> (
            let sheet_name =
              match Js.to_string sheet_name_input##value with
              | "" -> "Un-Named Sheet " ^ (string_of_float @@ Unix.time ())
              | _ as sn -> sn
            in
            ignore @@ %shell_print "\n\ncomplete_save_ss_handler:\n\n";
            ignore @@ %shell_print ("sheet_name = " ^ sheet_name);
            lwt msg =
              %Db_funs.save_or_update_sheet' (username, sheet_name, sheet_data)
            in
            modal##style##display <- Js.string "none";
            show_save_confirm_modal msg;
            Lwt.return_unit
          )
        );
        Js._true
    )

  let cancel_save_button modal =
    let btn = createButton document in
    btn##className <- Js.string "btn btn-danger";
    btn##id <- Js.string "CancelSaveBtn";
    btn##textContent <- Js.some @@ Js.string "Cancel";
    btn##onmouseup <- cancel_save_ss_handler modal;
    btn

  (* Appears in the save modal and completes the save *)
  let complete_save_button modal username sheet_name_input =
    let btn = createButton document in
    btn##className <- Js.string "btn btn-success";
    btn##id <- Js.string "CompleteSaveBtn";
    btn##textContent <- Js.some @@ Js.string "Save";
    btn##onmouseup <- complete_save_ss_handler modal username sheet_name_input (json_string_of_ss ());
    btn

  let save_modal ?sheet_name username =
    let title_div = createDiv document in
    title_div##id <- Js.string "save_modal_title_div";
    title_div##textContent <- Js.some @@ Js.string "Save Spreadsheet";
    let div = createDiv document in
    div##id <- Js.string "save_modal";
    div##className <- Js.string "modal";
    let name_div = createDiv document in
    name_div##id <- Js.string "save_modal_name_div";
    let name_label = createDiv document in
    name_label##id <- Js.string "save_modal_name_label";
    name_label##textContent <- Js.some @@ Js.string "Name";
    let name_input = createInput ~_type:(Js.string "text") document in
    name_input##id <- Js.string "save_modal_name_input";
    name_input##className <- Js.string "form-control";
    name_input##style##width <- Js.string "100%";
    name_input##placeholder <- Js.string "Spreadsheet Name...";
    (
      match sheet_name with
      | None -> ()
      | Some s -> name_input##value <- s
    );
    let description_div = createDiv document in
    description_div##id <- Js.string "save_modal_description_div";
    let description_input = createInput ~_type:(Js.string "text") document in
    description_input##id <- Js.string "save_modal_description_input";
    description_input##className <- Js.string "form-control";
    description_input##style##width <- Js.string "100%";
    description_input##placeholder <- Js.string "Description...";
    appendChild name_div name_label;
    appendChild name_div name_input;
    appendChild description_div description_input;
    appendChild div title_div;
    appendChild div name_div;
    appendChild div description_div;
    appendChild div (cancel_save_button div);
    appendChild div (complete_save_button div username name_input);
    appendChild document##body div

  let save_ss_modal_handler ?sheet_name username =
    handler (fun _ -> save_modal ?sheet_name username; Js._true)

  let save_button ?sheet_name username =
    let div = createDiv document in
    let btn = createButton document in
    btn##className <- Js.string "glyphicon glyphicon-floppy-disk";
    btn##id <- Js.string "SaveBtn";
    div##id <- Js.string "SaveBtnDiv";
    btn##onmouseup <- save_ss_modal_handler ?sheet_name username;
    appendChild div btn;
    div

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

  let un_border_top_row ?style () =
    try (
      match selected_area_top_row () with
      | [] -> ()
      | l  ->
        List.iter (fun c ->
            let cl = getElementById (id_of_cell c) in
            cl##style##display <- Js.string "none";
            cl##style##borderTop <- Js.string (existing_border_style ?style ~side:`Top cl);
            cl##style##display <- Js.string "table-cell"
          ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

  let un_border_bottom_row () =
    try (
      match selected_area_bottom_row () with
      | [] -> ()
      | l  ->
        List.iter (fun c ->
            let cl = getElementById (id_of_cell c) in
            cl##style##display <- Js.string "none";
            cl##style##borderBottom <- Js.string "0px";
            cl##style##borderBottomWidth <- Js.string "0px";
            cl##style##display <- Js.string "table-cell"
          ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

  let un_border_left_col ?style () =
    try (
      match selected_area_left_col () with
      | [] -> ()
      | l  ->
          List.iter (fun c ->
            let cl = getElementById (id_of_cell c) in
            cl##style##display <- Js.string "none";
            cl##style##borderLeft <- Js.string (existing_border_style ?style ~side:`Left cl);
            cl##style##display <- Js.string "table-cell"
          ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

  let un_border_right_col () =
    try (
      match selected_area_right_col () with
      | [] -> ()
      | l  ->
          List.iter (fun c ->
            let cl = getElementById (id_of_cell c) in
            cl##style##display <- Js.string "none";
            cl##style##borderRight <- Js.string "0px";
            cl##style##display <- Js.string "table-cell"
          ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

  let un_border_all () =
    un_border_top_row ();
    un_border_bottom_row ();
    un_border_left_col ();
    un_border_right_col ()

  let border_top_row ?(store = false) ?(style = "2px solid black") () =
    try (
      match selected_area_top_row () with
      | [] -> ()
      | l  -> List.iter (fun c ->
          let cl = getElementById (id_of_cell c) in
          cl##style##display <- Js.string "none";
          cl##style##borderTop <- Js.string style;
          cl##style##display <- Js.string "table-cell";
          if store
          then update_cell_border_in_h ~side:`Top (key_of_id @@ Js.to_string cl##id) style
          else ()
        ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

let border_bottom_row ?(style = "1px solid black") () =
  try (
      match selected_area_bottom_row () with
      | [] -> ()
      | l  ->
          List.iter (fun c ->
            let cl = getElementById (id_of_cell c) in
            cl##style##display <- Js.string "none";
            cl##style##borderBottom <- Js.string style;
            cl##style##display <- Js.string "table-cell"
          ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

  let border_left_col ?(store = false) ?(style = "2px solid black") () =
    try (
      match selected_area_left_col () with
      | [] -> ()
      | l  ->
          List.iter (fun c ->
            let cl = getElementById (id_of_cell c) in
            cl##style##display <- Js.string "none";
            cl##style##borderLeft <- Js.string style;
            cl##style##display <- Js.string "table-cell";
            if store
            then update_cell_border_in_h ~side:`Left (key_of_id @@ Js.to_string cl##id) style
            else ()
          ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

  let border_right_col ?(style = "1px solid black") () =
    try (
      match selected_area_right_col () with
      | [] -> ()
      | l  ->
          List.iter (fun c ->
            let cl = getElementById (id_of_cell c) in
            cl##style##display <- Js.string "none";
            cl##style##borderRight <- Js.string style;
            cl##style##display <- Js.string "table-cell"
          ) l
    )
    with
    | _ -> () (* TODO: Log Error *)

  let border_all () =
    border_top_row ();
    border_bottom_row ();
    border_left_col ();
    border_right_col ()

  let highlight_cells cl =
    List.iter (fun c ->
        try
          let td = getElementById (id_of_cell c) in
          td##style##backgroundColor <- Js.string "rgba(102, 175, 233, 0.5)"
        with _ -> () (* TODO: Log error here *)
      ) cl

  let unhighlight_cells cl =
    List.iter (fun c ->
        try (
          let td = getElementById (id_of_cell c) in
          if Hashtbl.mem h (key_of_id @@ id_of_cell c)
          then (
            let old_color = color_of_cell @@ get_cell (key_of_id @@ id_of_cell c) in
            td##style##backgroundColor <- Js.string old_color
          )
          else (
            td##style##backgroundColor <- Js.string cell_background_color
          )
        )
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
      sc##style##backgroundColor <- Js.string "rgba(102, 175, 233, 0.5)";
      selected_area := (
        match !selected_cell with
        | None -> None
        | Some c -> Some [c]
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
  (* TODO: Up arrow action does not aways re-draw the bottom border of the last cell correctly*)
  let up_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        un_border_all ();
        unhighlight_selected_area ();
        selected_area := None;
        match up_cell () with
        | None -> ()
        | Some up_c ->
            let sc = getElementById @@ id_of_cell sel_c in
            sc##style##display      <- Js.string "none";
            sc##style##borderTop    <- Js.string @@ existing_border_style ~side:`Top sc;
            sc##style##borderBottom <- Js.string @@ existing_border_style ~side:`Bottom sc;
            sc##style##borderLeft   <- Js.string @@ existing_border_style ~side:`Left sc;
            sc##style##borderRight  <- Js.string @@ existing_border_style ~side:`Right sc;
            sc##style##display      <- Js.string "table-cell";
            let uc = getElementById @@ id_of_cell up_c in
            selected_cell := (Some up_c);
            uc##style##display           <- Js.string "none";
            uc##style##borderTop         <- Js.string "3px solid black";
            uc##style##borderBottom      <- Js.string "2px solid black";
            uc##style##borderLeft        <- Js.string "3px solid black";
            uc##style##borderRight       <- Js.string "2px solid black";
            uc##style##display           <- Js.string "table-cell"
      )
    | Some sel_c, true -> (
        un_border_all (); (* TODO: poor solution for now *)
        update_selected_area_up ();
        highlight_selected_area ();
        border_all () (* TODO: poor solution for now *)
      )
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a down arrow *)
  let down_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        un_border_all ();
        unhighlight_selected_area ();
        selected_area := None;
        match down_cell () with
        | None -> ()
        | Some down_c ->
            let sc = getElementById @@ id_of_cell sel_c in
            sc##style##display      <- Js.string "none";
            sc##style##borderTop    <- Js.string @@ existing_border_style ~side:`Top sc;
            sc##style##borderBottom <- Js.string @@ existing_border_style ~side:`Bottom sc;
            sc##style##borderLeft   <- Js.string @@ existing_border_style ~side:`Left sc;
            sc##style##borderRight  <- Js.string @@ existing_border_style ~side:`Right sc;
            sc##style##display      <- Js.string "table-cell";
            let dc = getElementById @@ id_of_cell down_c in
            selected_cell := (Some down_c);
            dc##style##display      <- Js.string "none";
            dc##style##borderTop    <- Js.string "3px solid black";
            dc##style##borderBottom <- Js.string "2px solid black";
            dc##style##borderLeft   <- Js.string "3px solid black";
            dc##style##borderRight  <- Js.string "2px solid black";
            dc##style##display      <- Js.string "table-cell"
      )
    | Some sel_c, true -> (
        un_border_all ();
        update_selected_area_down ();
        highlight_selected_area ();
        border_all ()
      )
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a left arrow *)
  let left_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        un_border_all ();
        unhighlight_selected_area ();
        selected_area := None;
        match left_cell () with
        | None -> ()
        | Some left_c ->
            let sc = getElementById @@ id_of_cell sel_c in
            sc##style##display      <- Js.string "none";
            sc##style##borderTop    <- Js.string @@ existing_border_style ~side:`Top sc;
            sc##style##borderBottom <- Js.string @@ existing_border_style ~side:`Bottom sc;
            sc##style##borderLeft   <- Js.string @@ existing_border_style ~side:`Left sc;
            sc##style##borderRight  <- Js.string @@ existing_border_style ~side:`Right sc;
            sc##style##display      <- Js.string "table-cell";
            let lc = getElementById @@ id_of_cell left_c in
            selected_cell := (Some left_c);
            lc##style##display      <- Js.string "none";
            lc##style##borderTop    <- Js.string "3px solid black";
            lc##style##borderBottom <- Js.string "2px solid black";
            lc##style##borderLeft   <- Js.string "3px solid black";
            lc##style##borderRight  <- Js.string "2px solid black";
            lc##style##display      <- Js.string "table-cell"
      )
    | Some sel_c, true -> (
        un_border_all ();
        update_selected_area_left ();
        highlight_selected_area ();
        border_all ()
      )
    | None, _ -> () (* Note: This case should never happen *)

  (* Actions for a right arrow *)
  let right_arrow_action () =
    match !selected_cell, !shift_pressed with
    | Some sel_c, false -> (
        un_border_all ();
        unhighlight_selected_area ();
        selected_area := None;
        match right_cell () with
        | None -> ()
        | Some right_c ->
            let sc = getElementById @@ id_of_cell sel_c in
            sc##style##display      <- Js.string "none";
            sc##style##borderTop    <- Js.string @@ existing_border_style ~side:`Top sc;
            sc##style##borderBottom <- Js.string @@ existing_border_style ~side:`Bottom sc;
            sc##style##borderLeft   <- Js.string @@ existing_border_style ~side:`Left sc;
            sc##style##borderRight  <- Js.string @@ existing_border_style ~side:`Right sc;
            sc##style##display      <- Js.string "table-cell";
            let rc = getElementById @@ id_of_cell right_c in
            selected_cell := (Some right_c);
            rc##style##display      <- Js.string "none";
            rc##style##borderTop    <- Js.string "3px solid black";
            rc##style##borderBottom <- Js.string "2px solid black";
            rc##style##borderLeft   <- Js.string "3px solid black";
            rc##style##borderRight  <- Js.string "2px solid black";
            rc##style##display      <- Js.string "table-cell"
      )
    | Some sel_c, true -> (
        un_border_all ();
        update_selected_area_right ();
        highlight_selected_area ();
        border_all ()
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
        | 16 -> (shift_release_action (); Js._true)
        | _ -> Js._true
      )

  (* Colorpicker *)
  let color_selected_area color =
    match !selected_cell, !selected_area with
    | Some sc, None -> (
        try (
          let td = getElementById (id_of_cell sc) in
          td##style##backgroundColor <- Js.string color;
          update_cell_color_in_h (key_of_id @@ Js.to_string td##id) color
        )
        with _ -> () (* TODO: Log error here *)
      )
    | _, Some sa ->
        List.iter (fun c ->
          try (
            let td = getElementById (id_of_cell c) in
            td##style##backgroundColor <- Js.string color;
            update_cell_color_in_h (key_of_id @@ Js.to_string td##id) color
          )
          with _ -> () (* TODO: Log error here *)
        ) sa
    | None, None -> ()

  let color_cells_btn  (clr_pkr : inputElement Js.t) =
    let div = createDiv document in
    let btn = createButton document in
    (*btn##textContent <- Js.some @@ Js.string "Highlight";*)
    btn##className <- Js.string "glyphicon glyphicon-tint";
    btn##id <- Js.string "ColorPickerBtn";
    div##id <- Js.string "ColorPickerBtnDiv";
    btn##onmouseup <-
      handler (fun _ -> color_selected_area (Js.to_string clr_pkr##value); Js._true);
    appendChild div btn;
    div

  (* TODO: In addition to the color picker, need to be able to change the font colors *)

  let color_picker () =
    let inp =
      createInput ~_type:(Js.string "color") ~name:(Js.string "ColorPicker") document
    in
    inp##id <- Js.string "ColorPicker";
    inp

  (* Wrap the color picker input in a div for proper spacing on the page *)
  let color_picker_div cp =
    let div = createDiv document in
    div##id <- Js.string "ColorPickerDiv";
    appendChild div cp;
    div

  (* Make cell text bold *)
  let bold_button () =
    let div = createDiv document in
    let btn = createButton document in
    btn##className <- Js.string "glyphicon glyphicon-bold";
    btn##id <- Js.string "BoldBtn";
    div##id <- Js.string "BoldBtnDiv";
    btn##onmouseup <- handler (fun _ ->
        match !selected_area, !selected_cell with
        | None, None -> Js._true
        | None, Some sel_c -> (
              let td, td_id =
                match sel_c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##fontWeight = Js.string "bold"
              then (
                td##style##fontWeight <- Js.string "normal";
                update_cell_font_weight_in_h (key_of_id td_id) "normal"
              )
              else (
                td##style##fontWeight <- Js.string "bold";
                update_cell_font_weight_in_h (key_of_id td_id) "bold"
              );
              Js._true
          )
        | Some sa, _ -> (
            List.iter (fun (c : cell) ->
              let td, td_id =
                match c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##fontWeight = Js.string "bold"
              then (
                td##style##fontWeight <- Js.string "normal";
                update_cell_font_weight_in_h (key_of_id td_id) "normal"
              )
              else (
                td##style##fontWeight <- Js.string "bold";
                update_cell_font_weight_in_h (key_of_id td_id) "bold"
              )
            ) sa;
          Js._true
          )
      );
    appendChild div btn;
    div

  (* Make cell text italisized *)
  let italic_button () =
    let div = createDiv document in
    let btn = createButton document in
    btn##className <- Js.string "glyphicon glyphicon-italic";
    btn##id <- Js.string "ItalicBtn";
    div##id <- Js.string "ItalicBtnDiv";
    btn##onmouseup <- handler (fun _ ->
        match !selected_area, !selected_cell with
        | None, None -> Js._true
        | None, Some sel_c -> (
              let td, td_id =
                match sel_c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##fontStyle = Js.string "italic"
              then (
                td##style##fontStyle <- Js.string "normal";
                update_cell_font_style_in_h (key_of_id td_id) "normal"
              )
              else (
                td##style##fontStyle <- Js.string "italic";
                update_cell_font_style_in_h (key_of_id td_id) "italic"
              );
              Js._true
          )
        | Some sa, _ -> (
            List.iter (fun (c : cell) ->
              let td, td_id =
                match c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##fontStyle = Js.string "italic"
              then (
                td##style##fontStyle <- Js.string "normal";
                update_cell_font_style_in_h (key_of_id td_id) "normal"
              )
              else (
                td##style##fontStyle <- Js.string "italic";
                update_cell_font_style_in_h (key_of_id td_id) "italic"
              )
            ) sa;
          Js._true
          )
      );
    appendChild div btn;
    div

  (* Center the text *)
  let center_text_button () =
    let div = createDiv document in
    let btn = createButton document in
    btn##className <- Js.string "glyphicon glyphicon-align-center";
    btn##id <- Js.string "CenterTxtBtn";
    div##id <- Js.string "CenterTxtBtnDiv";
    btn##onmouseup <- handler (fun _ ->
        match !selected_area, !selected_cell with
        | None, None -> Js._true
        | None, Some sel_c -> (
              let td, td_id =
                match sel_c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##textAlign = Js.string "center"
              then (
                td##style##textAlign <- Js.string "initial";
                update_cell_text_align_in_h (key_of_id td_id) "initial"
              )
              else (
                td##style##textAlign <- Js.string "center";
                update_cell_text_align_in_h (key_of_id td_id) "center"
              );
              Js._true
          )
        | Some sa, _ -> (
            List.iter (fun (c : cell) ->
              let td, td_id =
                match c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##textAlign = Js.string "center"
              then (
                td##style##textAlign <- Js.string "initial";
                update_cell_text_align_in_h (key_of_id td_id) "initial"
              )
              else (
                td##style##textAlign <- Js.string "center";
                update_cell_text_align_in_h (key_of_id td_id) "center"
              )
            ) sa;
          Js._true
          )
      );
    appendChild div btn;
    div

  (* Left align the text *)
  let left_align_text_button () =
    let div = createDiv document in
    let btn = createButton document in
    btn##className <- Js.string "glyphicon glyphicon-align-left";
    btn##id <- Js.string "LeftAlignTxtBtn";
    div##id <- Js.string "LeftAlignTxtBtnDiv";
    btn##onmouseup <- handler (fun _ ->
        match !selected_area, !selected_cell with
        | None, None -> Js._true
        | None, Some sel_c -> (
              let td, td_id =
                match sel_c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##textAlign = Js.string "left"
              then (
                td##style##textAlign <- Js.string "initial";
                update_cell_text_align_in_h (key_of_id td_id) "initial"
              )
              else (
                td##style##textAlign <- Js.string "left";
                update_cell_text_align_in_h (key_of_id td_id) "left"
              );
              Js._true
          )
        | Some sa, _ -> (
            List.iter (fun (c : cell) ->
              let td, td_id =
                match c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##textAlign = Js.string "left"
              then (
                td##style##textAlign <- Js.string "initial";
                update_cell_text_align_in_h (key_of_id td_id) "initial"
              )
              else (
                td##style##textAlign <- Js.string "left";
                update_cell_text_align_in_h (key_of_id td_id) "left"
              )
            ) sa;
          Js._true
          )
      );
    appendChild div btn;
    div

  (* Right align the text *)
  let right_align_text_button () =
    let div = createDiv document in
    let btn = createButton document in
    btn##className <- Js.string "glyphicon glyphicon-align-right";
    btn##id <- Js.string "RightAlignTxtBtn";
    div##id <- Js.string "RightAlignTxtBtnDiv";
    btn##onmouseup <- handler (fun _ ->
        match !selected_area, !selected_cell with
        | None, None -> Js._true
        | None, Some sel_c -> (
              let td, td_id =
                match sel_c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##textAlign = Js.string "right"
              then (
                td##style##textAlign <- Js.string "initial";
                update_cell_text_align_in_h (key_of_id td_id) "initial"
              )
              else (
                td##style##textAlign <- Js.string "right";
                update_cell_text_align_in_h (key_of_id td_id) "right"
              );
              Js._true
          )
        | Some sa, _ -> (
            List.iter (fun (c : cell) ->
              let td, td_id =
                match c with
                | SingleCell sc -> getElementById sc.id, sc.id
                | MergedCell mc -> getElementById mc.id, mc.id
              in
              if td##style##textAlign = Js.string "right"
              then (
                td##style##textAlign <- Js.string "initial";
                update_cell_text_align_in_h (key_of_id td_id) "initial"
              )
              else (
                td##style##textAlign <- Js.string "right";
                update_cell_text_align_in_h (key_of_id td_id) "right"
              )
            ) sa;
          Js._true
          )
      );
    appendChild div btn;
    div

  (* Border selection dropdown *)
  (* NOTE: Only the top and left sides actually have a border *)
  let border_dropdown () =
    let jquery_script = createScript document in
    jquery_script##src <-
      Js.string "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js";

    let js_script = createScript document in
    js_script##src <-
      Js.string "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js";

    appendChild document##head jquery_script;
    appendChild document##head js_script;

    let div = createDiv document in
    div##className <- Js.string "dropdown";
    div##id <- Js.string "BorderDropdownDiv";

    let btn = createButton ?_type:(Some (Js.string "button")) document in
    btn##className <- Js.string "btn btn-default dropdown-toggle";
    btn##id <- Js.string "BorderDropdownBtn";
    btn##setAttribute((Js.string "data-toggle"), (Js.string "dropdown"));
    btn##innerHTML <- Js.string "Border ";

    let span = createSpan document in
    span##className <- Js.string "caret";

    let ul = createUl document in
    ul##className <- Js.string "dropdown-menu";

    let li_1 = createLi document in
    let no_border_btn = createButton document in
    no_border_btn##className <- Js.string "btn btn-default";
    no_border_btn##id <- Js.string "no_border_btn";
    no_border_btn##innerHTML <- Js.string "None";
    no_border_btn##onmousedown <- handler (fun clk ->
        if clk##button = 0
        then (
          border_top_row ~store:true ~style:"0px" ();
          border_left_col ~store:true ~style:"0px" ();
          List.iter (fun (c : cell) ->
              match c with
              | SingleCell sc -> (
                  (getElementById sc.id)##style##borderTop <- Js.string "0px";
                  update_cell_border_in_h ~side:`Top (key_of_id sc.id) "0px"
                )
              | MergedCell mc -> (
                  (getElementById mc.id)##style##borderTop <- Js.string "0px";
                  update_cell_border_in_h ~side:`Top (key_of_id mc.id) "0px"
                )
            ) (row_below_selected_area ());
          List.iter (fun (c : cell) ->
              match c with
              | SingleCell sc -> (
                  (getElementById sc.id)##style##borderLeft <- Js.string "0px";
                  update_cell_border_in_h ~side:`Left (key_of_id sc.id) "0px"
                )
              | MergedCell mc -> (
                  (getElementById mc.id)##style##borderLeft <- Js.string "0px";
                  update_cell_border_in_h ~side:`Left (key_of_id mc.id) "0px"
                )
            ) (col_right_selected_area ());
        )
        else ();
        Js._true
      );

    let li_2 = createLi document in
    let reg_border_btn = createButton document in
    reg_border_btn##className <- Js.string "btn btn-default";
    reg_border_btn##id <- Js.string "reg_border_btn";
    reg_border_btn##innerHTML <- Js.string "Regular";
    reg_border_btn##onmousedown <- handler (fun clk ->
        if clk##button = 0
        then (
          border_top_row ~store:true ~style:"1px solid black" ();
          border_left_col ~store:true ~style:"1px solid black" ();
          List.iter (fun (c : cell) ->
              match c with
              | SingleCell sc -> (
                  (getElementById sc.id)##style##borderTop <- Js.string "1px solid black";
                  update_cell_border_in_h ~side:`Top (key_of_id sc.id) "1px solid black"
                )
              | MergedCell mc -> (
                  (getElementById mc.id)##style##borderTop <- Js.string "1px solid black";
                  update_cell_border_in_h ~side:`Top (key_of_id mc.id) "1px solid black"
                )
            ) (row_below_selected_area ());
          List.iter (fun (c : cell) ->
              match c with
              | SingleCell sc -> (
                  (getElementById sc.id)##style##borderLeft <- Js.string "1px solid black";
                  update_cell_border_in_h ~side:`Left (key_of_id sc.id) "1px solid black"
                )
              | MergedCell mc -> (
                  (getElementById mc.id)##style##borderLeft <- Js.string "1px solid black";
                  update_cell_border_in_h ~side:`Left (key_of_id mc.id) "1px solid black"
                )
            ) (col_right_selected_area ());
        )
        else ();
        Js._true
      );

    let li_3 = createLi document in
    let thick_border_btn = createButton document in
    thick_border_btn##className <- Js.string "btn btn-default";
    thick_border_btn##id <- Js.string "thick_border_btn";
    thick_border_btn##innerHTML <- Js.string "Thick";
    thick_border_btn##onmousedown <- handler (fun clk ->
        if clk##button = 0
        then (
          border_top_row ~store:true ~style:"2px solid black" ();
          border_left_col ~store:true ~style:"2px solid black" ();
          List.iter (fun (c : cell) ->
              match c with
              | SingleCell sc -> (
                  (getElementById sc.id)##style##borderTop <- Js.string "2px solid black";
                  update_cell_border_in_h ~side:`Top (key_of_id sc.id) "2px solid black"
                )
              | MergedCell mc -> (
                  (getElementById mc.id)##style##borderTop <- Js.string "2px solid black";
                  update_cell_border_in_h ~side:`Top (key_of_id mc.id) "2px solid black"
                )
            ) (row_below_selected_area ());
          List.iter (fun (c : cell) ->
              match c with
              | SingleCell sc -> (
                  (getElementById sc.id)##style##borderLeft <- Js.string "2px solid black";
                  update_cell_border_in_h ~side:`Left (key_of_id sc.id) "2px solid black"
                )
              | MergedCell mc -> (
                  (getElementById mc.id)##style##borderLeft <- Js.string "2px solid black";
                  update_cell_border_in_h ~side:`Left (key_of_id mc.id) "2px solid black"
                )
            ) (col_right_selected_area ());
        )
        else ();
        Js._true
      );

    appendChild li_1 no_border_btn;
    appendChild ul li_1;

    appendChild li_2 reg_border_btn;
    appendChild ul li_2;

    appendChild li_3 thick_border_btn;
    appendChild ul li_3;

    appendChild btn span;
    appendChild div btn;
    appendChild div ul;
    div

  let merge_selected_area () =
    match !selected_area with
    | None -> ()
    | Some sa -> merge_area ~blank_cell:true sa

  let merge_cells_button () =
    let div = createDiv document in
    div##id <- Js.string "MergeCellsDiv";

    let btn = createButton ?_type:(Some (Js.string "button")) document in
    btn##onmouseup <- handler (fun _ -> merge_selected_area (); Js._true);
    btn##className <- Js.string "btn btn-default";
    btn##id <- Js.string "MergeCellsBtn";
    btn##innerHTML <- Js.string "Merge";

    appendChild div btn;
    div

  (* Toolbar with Buttons *)
  let toolbar ?username ?sheet_name () =
    let toolbar = createDiv document in
    toolbar##style##backgroundColor <- Js.string cell_background_color;
    toolbar##id <- Js.string "toolbar";
    let cp = color_picker () in
    (
      match username with
     | None -> ()
     | Some un -> appendChild toolbar (save_button ?sheet_name un)
    );
    appendChild toolbar (color_picker_div cp);
    appendChild toolbar (color_cells_btn cp);
    appendChild toolbar (bold_button ());
    appendChild toolbar (italic_button ());
    appendChild toolbar (left_align_text_button ());
    appendChild toolbar (center_text_button ());
    appendChild toolbar (right_align_text_button ());
    appendChild toolbar (border_dropdown ());
    appendChild toolbar (merge_cells_button ());
    toolbar

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
    td##style##borderTop       <- Js.string "1px solid black"; (* TODO: Put in a css class? *)
    td##style##borderBottom    <- Js.string "0px";               (* TODO: Put in a css class? *)
    td##style##borderLeft      <- Js.string "1px solid black"; (* TODO: Put in a css class? *)
    td##style##borderRight     <- Js.string "0px";               (* TODO: Put in a css class? *)
    td##id                     <- Js.string id;
    (*td##onkeyup                <- f2_handler td;*)
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

  let rec fresh_table ?(table_body = None) ?username ?sheet_name ~nrows ~ncols () =
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
      fresh_table ~table_body:(Some tbdy) ?username ~nrows ~ncols ()
    )
    else (
      max_row := nrows;
      max_col := ncols;
      let tbl = createTable document in
      (*tbl##width <- Js.string "80%";*)
      tbl##border <- Js.string "1";
      tbl##id <- Js.string "main_table";
      tbl##style##borderCollapse <- Js.string "collapse";
      let body = document##body in
      let tbl_div = createDiv document in
      tbl_div##id <- Js.string "tbl_div";
      body##onkeydown <- key_handler;
      body##onkeyup <- key_release_handler;
      (*appendChild tbl tbdy;
        appendChild body tbl*)
      appendChild tbl_div (toolbar ?username ?sheet_name ());
      appendChild tbl tbdy;
      appendChild tbl_div tbl;
      appendChild body tbl_div
    )


  (* Load a table populated with data *)
  let load_table_with_data ?username ~nrows ~ncols ss_string =
    fresh_table ?username ~nrows:num_sheet_rows ~ncols:num_sheet_cols ();
    let (kv_list : ((int * int) * cell) list) =
      parse_ss_string ss_string |> List.map (fun c -> ((key_of_cell c), c))
    in
    replace_h kv_list;
    Hashtbl.iter (fun (k : int * int) (v : cell) -> update_td k v) h

  let print_h () =
    ignore @@ %shell_print "Hashtbl h = \n";
    Hashtbl.iter (fun k c -> ignore @@ %shell_print @@ string_of_cell c) h

  let print_h_button () =
    ignore @@ %shell_print "\nprint_h_button called:\n";
    let btn = createButton document in
    btn##textContent <- Js.some @@ Js.string "Print Hashtable";
    btn##onmouseup <- handler (fun _ -> print_h (); Js._true);
    appendChild document##body btn

}}
