open Bogue

let quiz_answers = ref [||]

(* Save responses to file when submit is clicked *)
let save_responses_to_file () =
  try
    let response_file = "quiz_responses.tmp" in
    let oc = open_out_bin response_file in
    Marshal.to_channel oc !quiz_answers [Marshal.Closures];
    close_out oc
  with _ -> ()
let set_layout widget height = Layout.resident ~w:500 ~h:height widget

let rounded_panel ?(bg_color = Style.Solid Draw.(opaque white))
    ?(border_color = Draw.(transp white)) ?(border_width = 7) width height
    radius label =
  let style =
    Style.create ~background:bg_color
      ~border:
        (Style.mk_border ~radius
           (Style.mk_line ~color:Draw.(border_color) ~width:border_width ()))
      ()
  in
  Layout.superpose ~center:true
    [ Layout.resident (Widget.box ~w:width ~h:height ~style ()); label ]

let vertical_radiolist labels =
  let rl = Radiolist.vertical (Array.of_list labels) in
  let layout = Radiolist.layout rl in
  (rl, layout)

let get_blank width height =
  Layout.resident ~w:width ~h:height (Widget.label "")

let radio_question question answers blank =
  let radiolist, row_layout = vertical_radiolist answers in
  ( Layout.flat ~align:Draw.Center
      [
        get_blank 160 20;
        rounded_panel 700 160 10
          (Layout.tower ~align:Draw.Center ~hmargin:200
             [ set_layout question 20; Layout.flat [ row_layout; blank ] ]);
      ],
    radiolist )

let q1 () =
  let answer = Widget.text_input ~max_size:50 () in
  ( Layout.flat ~align:Draw.Center
      [
        get_blank 260 20;
        rounded_panel 700 110 10
          (Layout.tower ~align:Draw.Min ~hmargin:20
             [
               set_layout (Widget.text_display "1.  What is your name?") 20;
               Layout.flat [ get_blank 1 20; Layout.resident ~w:200 answer ];
             ]);
      ],
    answer )

let q2 () =
  let radiolist, row_layout =
    vertical_radiolist
      [
        "1. Growth (capital appreciation)";
        "2. Income (dividend/regular income)";
        "3. Balanced (growth and income)";
        "4. Preservation (protect capital)";
      ]
  in
  ( Layout.flat ~align:Draw.Center
      [
        get_blank 160 20;
        rounded_panel 700 180 10
          (Layout.tower ~align:Draw.Center ~hmargin:200
             [
               set_layout
                 (Widget.text_display
                    "2.  What is your primary investment goal?")
                 20;
               Layout.flat [ row_layout; get_blank 150 20 ];
             ]);
      ],
    radiolist )

let q3 () =
  radio_question
    (Widget.text_display "3.  How experienced are you with stock investing?")
    [ "1. Beginner"; "2. Intermediate"; "3. Experienced" ]
    (get_blank 280 20)

let q4 () =
  radio_question
    (Widget.text_display "4.  How would you describe your risk tolerance?")
    [
      "1. Conservative (low risk, stable returns)";
      "2. Moderate (balanced risk and return)";
      "3. Aggressive (high risk, higher potential returns)";
    ]
    (get_blank 70 20)

let q5 () =
  radio_question
    (Widget.text_display "5.  What is your investment time horizon?")
    [
      "1. Short-term (1-3 years)";
      "2. Medium-term (3-7 years)";
      "3. Long-term (7+ years)";
    ]
    (get_blank 210 20)

let q6 () =
  radio_question
    (Widget.text_display
       "6.  How many stocks would you like for diversification?")
    [
      "1. Small (3-5 stocks)";
      "2. Medium (5-10 stocks)";
      "3. Large (10+ stocks)";
    ]
    (get_blank 220 20)

let q7 () =
  let radiolist, row_layout = vertical_radiolist [ "Yes"; "No" ] in
  ( Layout.flat ~align:Draw.Center
      [
        get_blank 190 20;
        rounded_panel 700 130 10
          (Layout.tower ~align:Draw.Center ~hmargin:172
             [
               set_layout
                 (Widget.text_display
                    "7.  Are you willing to invest in assets that may temporarily lose \
                     money?")
                 20;
               Layout.flat [ row_layout; get_blank 345 20 ];
             ]);
      ],
    radiolist )

let q8 () =
  let answer = Widget.text_input ~max_size:1000 () in
  ( Layout.flat ~align:Draw.Center
      [
        get_blank 260 20;
        rounded_panel 700 110 10
          (Layout.tower ~align:Draw.Min ~hmargin:20
             [
               set_layout
                 (Widget.text_display
                    "8.  List current investments tickers (comma separated, leave blank if none).\n\
                     (ticker symbols, separated by commas, leave blank if none)")
                 20;
               Layout.flat [ get_blank 1 20; Layout.resident ~w:200 answer ];
             ]);
      ],
    answer )

let submit () =
  let submit =
    Widget.button ~border_radius:10 ~label:(Label.create ~size:15 "Submit") ""
  in
  ( submit,
    Layout.flat ~align:Draw.Center
      [ Layout.resident ~w:900 ~h:40 (Widget.label ""); Layout.resident submit ]
  )

let check_filled name radios =
  Widget.get_text name <> ""
  && List.for_all (fun r -> Radiolist.get_index r <> None) radios

let show_missing_popup layout =
  Popup.info ~w:200 ~h:50 "MISSING ANSWERS\nPlease answer all questions." layout

let set_answers name goal exp risk horizon size willing curr =
  let get_value x = string_of_int (Option.get (Radiolist.get_index x) + 1) in
  quiz_answers :=
    [|
      Widget.get_text name;
      get_value goal;
      get_value exp;
      get_value risk;
      get_value horizon;
      get_value size;
      get_value willing;
      Widget.get_text curr;
    |]

let build_heading () =
  Layout.flat ~align:Draw.Center
    [
      get_blank 37 20;
      rounded_panel 700 80 10
        (Layout.flat ~align:Draw.Min
           [
             get_blank 230 20;
             Layout.resident
               (Widget.label ~size:30 "   Please answer the following questions:");
             get_blank 330 20;
           ]);
    ]

let build_question_content name goal exp risk hor size will curr submit =
  Layout.tower
    [
      get_blank 700 20;
      Layout.flat ~align:Draw.Center
        [
          get_blank 258 5;
          rounded_panel
            ~bg_color:(Style.Solid Draw.(opaque (164, 211, 222)))
            ~border_color:Draw.(opaque (164, 211, 222))
            700 13 10 (get_blank 700 10);
        ];
      build_heading ();
      name;
      goal;
      exp;
      risk;
      hor;
      size;
      will;
      curr;
      submit;
    ]

let questions next_page =
  let name, input = q1 () in
  let goal, q2 = q2 () in
  let exp, q3 = q3 () in
  let risk, q4 = q4 () in
  let hor, q5 = q5 () in
  let size, q6 = q6 () in
  let will, q7 = q7 () in
  let curr, q8 = q8 () in
  let submit, submit_layout = submit () in
  let layout =
    Layout.make_clip ~h:500
      (build_question_content name goal exp risk hor size will curr
         submit_layout)
  in
  Space.full_width layout;
  Space.full_height layout;
  let submit_form _widget =
    if not (check_filled input [ q2; q3; q4; q5; q6; q7 ]) then
      show_missing_popup layout
    else (
      set_answers input q2 q3 q4 q5 q6 q7 q8;
      save_responses_to_file ();
      Layout.set_rooms layout [ next_page ])
  in
  Widget.on_button_release ~release:submit_form submit;
  Layout.flat ~background:(Layout.color_bg Draw.(211, 228, 232, 500)) [ layout ]

let build_page_content text_layout top top_border top_white bottom_border =
  Layout.tower ~align:Draw.Center ~sep:(-22)
    [
      rounded_panel
        ~bg_color:(Style.color_bg Draw.(211, 228, 232, 500))
        ~border_color:Draw.(211, 228, 232, 500)
        1175 top 0 (get_blank 5 5);
      rounded_panel
        ~bg_color:(Style.color_bg Draw.(opaque (240, 245, 246)))
        ~border_color:Draw.(opaque (240, 245, 246))
        1175 top_border 0 (get_blank 5 5);
      rounded_panel
        ~bg_color:(Style.color_bg Draw.(opaque white))
        ~border_color:Draw.(opaque white)
        1175 top_white 0 (get_blank 5 5);
      text_layout;
      rounded_panel
        ~bg_color:(Style.color_bg Draw.(opaque white))
        ~border_color:Draw.(opaque white)
        1175 bottom_border 0 (get_blank 5 5);
      rounded_panel
        ~bg_color:(Style.color_bg Draw.(opaque (240, 245, 246)))
        ~border_color:Draw.(opaque (240, 245, 246))
        1175 10 0 (get_blank 5 10);
    ]

let make_button button height =
  rounded_panel
    ~bg_color:(Style.Solid Draw.(opaque (213, 229, 233)))
    ~border_color:Draw.(opaque (213, 229, 233))
    1175 200 0
    (Layout.resident ~w:250 ~h:height button)

let homepage next_page =
  let button_label = Label.create ~size:32 "START" in
  let button = Widget.button ~border_radius:25 ~label:button_label "" in
  let layout =
    Layout.tower ~sep:(-11) ~align:Draw.Center
      [
        build_page_content
          (rounded_panel 1175 200 0
             (Layout.tower ~align:Draw.Center
                [
                  Layout.flat_of_w ~align:Draw.Center
                    [ Widget.label ~size:50 "=== Portfolio Optimizer Quiz ===" ];
                  Layout.flat_of_w ~align:Draw.Center
                    [
                      Widget.label ~size:32
                        "Welcome! Let's find the perfect portfolio for you.";
                    ];
                  Layout.flat_of_w ~align:Draw.Center
                    [ Widget.label ~size:32 "Click [START] to begin." ];
                ]))
          150 26 30 50;
        make_button button 85;
      ]
  in
  Space.full_width layout;
  Space.full_height layout;
  let switch_screens _widget = Layout.set_rooms layout [ next_page ] in
  Widget.on_button_release ~release:switch_screens button;
  Layout.flat ~hmargin:(-10)
    ~background:(Layout.color_bg Draw.(211, 228, 232, 500))
    [ layout ]

let endpage prev_page =
  let button_label = Label.create ~size:32 "EXIT" in
  let button = Widget.button ~border_radius:25 ~label:button_label "" in
  let layout =
    Layout.tower ~sep:(-11) ~align:Draw.Center
      [
        build_page_content
          (rounded_panel 1175 200 0
             (Layout.tower ~align:Draw.Center
                [
                  Layout.flat_of_w ~align:Draw.Center
                    [ Widget.label ~size:50 "Thank you!" ];
                  Layout.flat_of_w ~align:Draw.Center
                    [
                      Widget.label ~size:40
                        "Your responses have been recorded.";
                    ];
                ]))
          150 36 0 25;
        make_button button 75;
      ]
  in
  Space.full_width layout;
  Space.full_height layout;
  (* Auto-close on button click or after showing end page *)
  let end_quiz _widget = Trigger.push_quit () in
  Widget.on_button_release ~release:end_quiz button;
  Layout.flat ~align:Draw.Center ~hmargin:(-10)
    ~background:(Layout.color_bg Draw.(211, 228, 232, 500))
    [ layout ]

let run_gui () =
  let endpage = endpage () in
  Layout.set_size endpage ~w:1250 ~h:820;
  let questions = questions endpage in
  Layout.set_size questions ~w:1250 ~h:820;
  let homepage = homepage questions in
  Layout.set_size homepage ~w:1250 ~h:820;
  let board = Main.of_layout homepage in
  Main.run board

let get_responses () = !quiz_answers
