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

let () =
  SS_app.register
    ~service:main_service
    (fun () () ->
     let _ = {unit{fresh_table ~nrows:num_sheet_rows ~ncols:num_sheet_cols ()}} in
     let _ = {unit{save_button ()}} in
     let _ = {unit{load_button ()}} in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"SS"
           ~css:[["css";"SS.css"]]
           Html5.F.(body [
           ])))

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
