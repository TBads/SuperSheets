{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Cells
  open Eliom_parameter
}}

(* TODO Need a land page for the user, which lists all spreadsheets, etc... *)

open Types

(* Use for communication only on the Main Trading page, i.e. msg_box, active_orders, etc... *)
let user_info =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.default_process_scope ~secure:true
    {
      username = None;
      email = None;
      verified = None
    }

module SS_app =
  Eliom_registration.App (
    struct
      let application_name = "SS"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let test_service =
  Eliom_service.App.service ~path:["unittests"] ~get_params:Eliom_parameter.unit ()

let new_account_service =
  Eliom_service.Http.service ~path:["new_account"] ~get_params:Eliom_parameter.unit ()

(* Login Page Service *)
let login_service =
  Eliom_service.Http.service ~path:["login"] ~get_params:Eliom_parameter.unit ()

(* Logout Page Service *)
let logout_service =
  Eliom_service.Http.service ~path:["logout"] ~get_params:Eliom_parameter.unit ()

(* Verify the users login details and set the session data if username and password are verified *)
let login_verify_service =
  Eliom_service.Http.post_service ~fallback:login_service
                                  ~post_params:(string "username" ** string "password") ()

let test_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a test_service [pcdata "Unit Tests"] ()
  ]

let sign_up_button (u : user) =
  match u.verified with
  | Some true -> div []
  | _ ->
    div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
    [a new_account_service [pcdata "Sign Up"] ()
    ]

let infobox_sign_up_button (u : user) =
  match u.verified with
  | Some true -> div []
  | _ ->
    div ~a:[a_class ["btn btn-default btn-lg"]; a_id "infobox_signup_btn"]
    [a new_account_service [pcdata "Sign Up"] ()
    ]

let login_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a login_service [pcdata "Login"] ()
  ]

let logout_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a logout_service [pcdata "Logout"] ()
  ]

let login_logout_button (u : user) =
  match u.verified with
  | Some true -> logout_button
  | _ -> login_button

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

let header_navbar_skeleton ?on_page (u : user) =
  let b1 = if on_page = Some `SignUp then [] else [sign_up_button u] in
  (* TODO Some `NewAccount *)
  let b2 = [login_logout_button u] in
  let btns =
    match on_page with
    | Some `SignUp ->b2
    | Some `NewAccount -> b2
    | Some `Login -> b1 @ b2
    | Some `Logout -> b1
    | _ -> b1 @ b2
  in
  nav ~a:[a_class ["navbar navbar-fixed-top"]; a_style "background-color: #333;"]
  [div ~a:[a_class ["container-fluid"]]
   [div ~a:[a_class ["navbar-header"]] btns
   ]
  ]

(* Bootstrap CDN link *)
let bootstrap_cdn_link =
  let cdn_link = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" in
    link ~rel:[`Stylesheet] ~href:(Xml.uri_of_string cdn_link)
      ()

(* Write the New account info to the database *)
let new_acct_db_service =
  Eliom_service.Http.post_service ~fallback:new_account_service
                                  ~post_params:(string "new_username" **
                                                string "new_email" **
                                                string "new_password" **
                                                string "verify_new_password") ()
let new_account_form =
  Eliom_content.Html5.F.post_form ~service:new_acct_db_service ~port:Config.port
  (
    fun (new_username, (new_email, (new_password, verify_new_password))) ->
      [div ~a:[a_style "width: 600px; margin: auto"]
       [div ~a:[a_class ["panel panel-primary"];
                a_style "border: 1px solid #634271; width: 400px; margin: auto; border-radius: 4px"]
        [div ~a:[a_class ["panel-heading"];
                 a_style "background-color: #634271; border: 1px solid #634271; border-radius: 0px"]
         [h3 ~a:[a_class ["panel-title"; "text-center"]] [pcdata "Register New Account"]
         ];

         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [
          div ~a:[a_class ["form-group"]]
          [div ~a:[a_class ["input-group"]]
           [Raw.span ~a:[a_class ["input-group-addon"]]
            [Raw.span ~a:[a_class ["glyphicon glyphicon-user"]] []
            ];
            string_input ~a:[a_class ["form-control"]; a_placeholder "Username"]
                         ~input_type:`Text ~name:new_username ()
           ]
          ];

          div ~a:[a_class ["form-group"]]
          [div ~a:[a_class ["input-group"]]
           [Raw.span ~a:[a_class ["input-group-addon"]]
            [Raw.span ~a:[a_class ["glyphicon glyphicon-envelope"]] []
            ];
            string_input ~a:[a_class ["form-control"]; a_placeholder "Email Address"]
                         ~input_type:`Text ~name:new_email ()
           ]
          ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-flag"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "Password"]
                          ~input_type:`Password ~name:new_password ()
            ]
           ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-flag"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "Verify Password"]
                          ~input_type:`Password ~name:verify_new_password ()
            ]
           ];

          button ~a:[a_class ["btn btn-lg btn-success btn-block"];
                     a_style "width: 150px; margin: auto; background-color: #634271;
                              border-color: #634271; font-size: 16px"]
                 ~button_type:`Submit [pcdata "Submit"]
      ]]
      ]]
  )

let login_form =
  Eliom_content.Html5.F.post_form ~service:login_verify_service ~port:Config.port
  (
    fun (username, password) ->
      [div ~a:[a_id "login_form"]
       [div ~a:[a_class ["panel panel-primary"];
                a_style "border: 1px solid #634271; width: 400px; margin: auto; border-radius: 4px"]
        [div ~a:[a_class ["panel-heading"];
                 a_style "background-color: #634271; border: 1px solid #634271; border-radius: 0px"]
         [h3 ~a:[a_class ["panel-title"; "text-center"]] [pcdata "Login"]
         ];
         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [div ~a:[a_class ["form-group"]]
          [string_input ~a:[a_class ["form-control"]; a_placeholder "Username"]
                        ~input_type:`Text ~name:username ()
          ];
          div ~a:[a_class ["form-group"]]
          [string_input ~a:[a_class ["form-control"]; a_placeholder "Password"]
                        ~input_type:`Password ~name:password ()
          ];
          button ~a:[a_class ["btn btn-lg btn-success btn-block"];
                     a_style "width: 150px; margin: auto; background-color: #634271;
                              border-color: #634271; font-size: 16px"]
                 ~button_type:`Submit [pcdata "Login"]
         ]
        ]
       ]
      ]
  )

let () =
  SS_app.register
    ~service:main_service
    (fun () () ->
     let _ = {unit{fresh_table ~nrows:num_sheet_rows ~ncols:num_sheet_cols ()}} in
     let _ = {unit{save_button ()}} in
     let _ = {unit{load_button ()}} in
     let _ = {unit{merge_area_button ()}} in
     let _ = {unit{print_h_button ()}} in
     let user = Eliom_reference.Volatile.get user_info in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"SS"
           ~css:[["css";"SS.css"]]
           ~other_head:[bootstrap_cdn_link]
           Html5.F.(
             body
             [header_navbar_skeleton user;
              div ~a:[a_id "dark_section"]
              [h1 ~a:[a_id "main_page_header"] [pcdata "Super Sheets"];
               h3 ~a:[a_id "tagline"] [pcdata ("Cloud based automated spreadsheet reporting")];
               div ~a:[a_id "infobox"]
               [h4 ~a:[a_id "infobox_header"] [pcdata ("Free 30 day Trial")];
                infobox_sign_up_button user
               ];
              ]
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

(* New Account Service *)
let () =
  Eliom_registration.Html5.register
    ~service:new_account_service
    (fun () () ->
      (* Kick off the thread *)
      Lwt.return @@ Eliom_reference.Volatile.get user_info
      >>= fun user ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Create New Account"
           ~css:[["css";"SS.css"]]
           ~other_head:[bootstrap_cdn_link]
           (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton ~on_page:`SignUp user;
            div ~a:[a_style "margin-top: 80px"]
            [h5 ~a:[a_style "text-align: center; margin-bottom: 30px"]
             [pcdata ("Your password must have at least 8 characters, and no spaces.")];
             new_account_form ()
            ]
           ])))

(* New Account Database Service - write the new user to the database *)
let () =
  Eliom_registration.Html5.register
    ~service:new_acct_db_service
    (fun () (new_username, (new_email, (new_password, verify_new_password))) ->
      (* Kick off the thread *)
      lwt username_taken = Db_funs.username_exists new_username in
      lwt email_taken =
        if new_email = "" then (Lwt.return false) else Db_funs.email_exists new_email
      in
      let password_verified = (new_password = verify_new_password) in
      let pwd_complexity, pwd_complexity_msg = Db_funs.pwd_req_check new_password in
      lwt user_registration_msg =
        if (not username_taken) && (not email_taken) && password_verified && pwd_complexity
        then
          (
            let new_user =
              {
                username = Some new_username;
                email    = Some new_email;
                verified = Some false
              }
            in
            let msg = Db_funs.write_new_user new_user new_password in
            (* It is ok to force verified=true since it is only for pub addr creation *)
            let new_user' =
              {
                username = new_user.username;
                email    = new_user.email;
                verified = Some true
              }
            in
            Eliom_reference.Volatile.set user_info new_user';
            msg
          )
        else
          Lwt.return
          (
            match username_taken, email_taken, password_verified, pwd_complexity with
            | true, _, _, _ -> "The username is not available."
            | false, true, _, _ -> "The email address is already registered."
            | false, false, false, _ -> "The password fields do not match"
            | false, false, true, false -> pwd_complexity_msg
            | false, false, true, true -> "Error: Please contact exchange support."
          )
      in
      let user = Eliom_reference.Volatile.get user_info in
      if (not username_taken) && (not email_taken) && password_verified && pwd_complexity
      then begin
        Lwt.return
          (Eliom_tools.F.html
             ~title:"Create New Account"
             ~css:[["css"; "SS.css"]]
             ~other_head:[bootstrap_cdn_link]
             (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton ~on_page:`NewAccount user;
              div ~a:[a_class ["margin_top_50px"; "padding_top_50px"]]
              [h3 ~a:[a_style "margin: auto auto 20px; text-align: center"]
               [pcdata ("Thanks for registering! Click the button below to begin trading.")];
               div ~a:[a_id "trade_btn_1_div"]
               [(*h1 [pcdata user_registration_msg];*)
                div ~a:[a_class ["btn btn-lg btn-success"]; a_id "trade_btn_1";
                        a_style "background-color: #634271; border-color: #634271"]
                [pcdata "Begin Trading"]
               ];
              ]
             ]))
      end
      else
        Lwt.return
          (Eliom_tools.F.html
             ~title:"Create New Account"
             ~css:[["css"; "SS.css"]]
             ~other_head:[bootstrap_cdn_link]
             (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton ~on_page:`NewAccount user;
              div ~a:[a_class ["margin_top_50px"; "padding_top_50px"]; a_id "registration_fail"]
              [h2 [pcdata user_registration_msg];
              ]
             ]))
          )

(* Login Service *)
let () =
  Eliom_registration.Html5.register
    ~service:login_service
    (fun () () ->
      (* Kick off the thread *)
      Lwt.return @@ Eliom_reference.Volatile.get user_info
      >>= fun user ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Login"
           ~css:[["css"; "SS.css"]]
           ~other_head:[bootstrap_cdn_link]
           (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton ~on_page:`Login user;
            login_form ()
           ])))

(* Logout Service *)
let () =
  Eliom_registration.Html5.register
    ~service:logout_service
    (fun () () ->
      (* Kick off the thread *)
      Lwt.return @@ Eliom_reference.Volatile.set user_info
        {
          username = None;
          email = None;
          verified = None
        }
      >>= fun () -> Lwt.return @@ Eliom_reference.Volatile.get user_info
      >>= fun user ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Logout"
           ~css:[["css"; "SS.css"]]
           ~other_head:[bootstrap_cdn_link]
           (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton ~on_page:`Logout user;
            div ~a:[a_class ["container"; "margin_top_50px"; "padding_top_50px"]]
            [h2 [pcdata "Logout Successful"];
            ]
           ])))

(* Verify the users login data and set the session data if the verification passes *)
let () =
  Eliom_registration.Redirection.register
    ~service:login_verify_service
    (fun () (username, password) ->
      (* Verify the user *)
      Db_funs.verify_login username password
      >>= fun b ->
      if b
      then (
        Eliom_reference.Volatile.set user_info
          {
            username = Some username;
            email = None;
            verified = Some true
          };
        Lwt.return main_service
      )
      else Lwt.return logout_service
    )

