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

{client{

  open Dom
  open Dom_html

  (* Build a fresh row as a JS Dom element *)
  let rec fresh_row ?(row = None) ~row_num ~ncols () =
    match row with
    | None ->
      (* Initialize the row and append the row number and an empty td *)
      let init_row = createTr document in
      let rn_td = createTd document in
      rn_td##textContent <- (Js.some @@ Js.string @@ string_of_int row_num);
      appendChild init_row rn_td;
      appendChild init_row (createTd document);
      fresh_row ~row:(Some init_row) ~row_num ~ncols ()
    | Some r ->
      if r##cells##length < ncols + 1 (* +1 for row numbering column *)
      then (
        appendChild r (createTd document);
        fresh_row ~row:(Some r) ~row_num ~ncols ()
      )
      else r

  let rec test_header_row ?(row = None) ~ncols () =
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
      test_header_row ~row:(Some init_row) ~ncols ()
    | Some r ->
      if r##cells##length < ncols + 1 (* + 1 for row numbering column *)
      then (
        appendChild r (fresh_td r##cells##length);
        test_header_row ~row:(Some r) ~ncols ()
      )
      else r

  let rec fresh_table ?(table_body = None) ~nrows ~ncols () =
    let tbdy =
      match table_body with
      | None ->
        let t = createTbody document in
        appendChild t (test_header_row ~ncols ());
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

  (* TODO: Function to change the value of a cell *)
}}

let () =
  SS_app.register
    ~service:main_service
    (fun () () ->
      let _ = {unit{fresh_table ~nrows:10 ~ncols:10 ()}} in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"SS"
           ~css:[["css";"SS.css"]]
           Html5.F.(body [
               h2 [pcdata "Second Goal: Click and change cell values!"]
           ])))
