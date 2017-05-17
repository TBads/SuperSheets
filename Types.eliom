{server{

  type user = {
    username : string option;
    email    : string option;
    verified : bool option
  }

let user_info =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.default_session_scope ~secure:true
    {
      username = None;
      email = None;
      verified = None
    }

}}

{client{

  (* NOTE: In single_cell and merged_cell, txt_in is the text input by the user and txt is the *)
  (*  text shown in the cell. These will be different if the user has typed in a formula.      *)
  (*  Ex. If txt_in = "=max(3,5)" then txt = "5"                                               *)
  type single_cell = {
    row           : int;
    col           : int;
    id            : string;
    txt_in        : string;
    txt           : string;
    color         : string;
    border_top    : string;
    border_bottom : string;
    border_left   : string;
    border_right  : string;
    text_align    : string;
    font_weight   : string;
    font_style    : string
  }

  type merged_cell = {
    top_row       : int;
    bottom_row    : int;
    left_col      : int;
    right_col     : int;
    id            : string;
    txt_in        : string;
    txt           : string;
    color         : string;
    border_top    : string;
    border_bottom : string;
    border_left   : string;
    border_right  : string;
    text_align    : string;
    font_weight   : string;
    font_style    : string
  }

  type cell = SingleCell of single_cell | MergedCell of merged_cell

  (* Keys take the form of (row,col), ex. row 1 column 3 has the key "1_3" *)
  let h : ((int * int), cell) Hashtbl.t = Hashtbl.create 100

}}
