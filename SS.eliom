{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Cells
}}

module SS_app =
  Eliom_registration.App (
    struct
      let application_name = "SS"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let test_service =
  Eliom_service.App.service ~path:["unittests"] ~get_params:Eliom_parameter.unit ()

let test_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a test_service [pcdata "Unit Tests"] ()
  ]

(* TODO Add a button to instert a button into the selected area *)

(* TODO: Add a dropdown of things the user can do with the selected region *)
(*let dropdown =
  ul ~a:[a_class ["nav nav-pills"]]
    [li ~a:[a_role ["presentation"]; a_class ["dropdown"]]
       [a
          ~a:[a_role["button"];
              a_class ["dropdown-toggle"];
              a_data_toggle ["dropdown"];
              a_href ["#"];
              a_aria_haspopup ["true"];
                a_aria_expanded ["false"]]
          [pcdata "Dropdown"]
       ]
    ]
*)

let header_navbar_skeleton =
  nav ~a:[a_class ["navbar navbar-fixed-top"]; a_style "background-color: #333;"]
  [div ~a:[a_class ["container-fluid"]]
   [div ~a:[a_class ["navbar-header"]]
    [test_button(*; dropdown*)
    ]
   ]
  ]

(* Bootstrap CDN link *)
let bootstrap_cdn_link =
  let cdn_link = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" in
    link ~rel:[`Stylesheet] ~href:(Xml.uri_of_string cdn_link)
      ()

let () =
  SS_app.register
    ~service:main_service
    (fun () () ->
     let _ = {unit{fresh_table ~nrows:num_sheet_rows ~ncols:num_sheet_cols ()}} in
     let _ = {unit{save_button ()}} in
     let _ = {unit{load_button ()}} in
     let _ = {unit{merge_area_button ()}} in
     let _ = {unit{print_h_button ()}} in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"SS"
           ~css:[["css";"SS.css"]]
           ~other_head:[bootstrap_cdn_link]
           Html5.F.(
             body ~a:[a_id "sheet"]
             [header_navbar_skeleton
             ]
           )
        )
    )

(* Unit test service *)
let () =
  SS_app.register
    ~service:test_service
    (fun () () ->
      let _ = {unit{fresh_table ~nrows:num_sheet_rows ~ncols:num_sheet_cols ()}} in
      let _ = {unit{UnitTests.run_tests ()}} in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Unit Tests"
           Html5.F.(body [
           ])))
