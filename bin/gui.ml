open Bogue

let quiz_answers = ref []
let set_layout widget height = Layout.resident ~w:500 ~h:height widget

let rounded_panel ?(bg_color = Style.Solid Draw.(opaque white))
    ?(border_color = Draw.(transp white)) ?(border_width = 7) width height
    radius label =
  let style =
    let border =
      Style.mk_border ~radius
        (Style.mk_line ~color:Draw.(border_color) ~width:border_width ())
    in
    Style.create ~background:bg_color ~border ()
  in
  let rounded_box = Layout.resident (Widget.box ~w:width ~h:height ~style ()) in
  Layout.superpose ~center:true [ rounded_box; label ]

let vertical_radiolist labels =
  let rl = Radiolist.vertical (Array.of_list labels) in
  let layout = Radiolist.layout rl in
  (rl, layout)

let get_blank width height =
  Layout.resident ~w:width ~h:height (Widget.label "")

let radio_question question answers blank =
  let question_layout = set_layout question 20 in
  let radiolist, row_layout = vertical_radiolist answers in
  let answer = Layout.flat [ row_layout; blank ] in
  let rounded =
    rounded_panel 700 160 10
      (Layout.tower ~align:Draw.Center ~hmargin:200 [ question_layout; answer ])
  in
  let full_layout =
    Layout.flat ~align:Draw.Center [ get_blank 160 20; rounded ]
  in
  (full_layout, radiolist)

let q1 () =
  let question_layout =
    set_layout (Widget.text_display "1.  What is your name?") 20
  in
  let answer = Widget.text_input ~max_size:50 () in
  let answer_layout =
    Layout.flat [ get_blank 1 20; Layout.resident ~w:200 answer ]
  in
  let rounded =
    rounded_panel 700 110 10
      (Layout.tower ~align:Draw.Min ~hmargin:20
         [ question_layout; answer_layout ])
  in
  let full_layout =
    Layout.flat ~align:Draw.Center [ get_blank 260 20; rounded ]
  in
  (full_layout, answer)

let q2 () =
  let question =
    Widget.text_display "2.  What is your primary investment goal?"
  in
  let question_layout = set_layout question 20 in
  let radiolist, row_layout =
    vertical_radiolist
      [
        "1. Growth (capital appreciation)";
        "2. Income (dividend/regular income)";
        "3. Balanced (growth and income)";
        "4. Preservation (protect capital)";
      ]
  in
  let answer = Layout.flat [ row_layout; get_blank 150 20 ] in
  let rounded =
    rounded_panel 700 180 10
      (Layout.tower ~align:Draw.Center ~hmargin:200 [ question_layout; answer ])
  in
  let full_layout =
    Layout.flat ~align:Draw.Center [ get_blank 160 20; rounded ]
  in
  (full_layout, radiolist)

let q3 () =
  let question =
    Widget.text_display "3.  How experienced are you with stock investing?"
  in
  radio_question question
    [ "1. Beginner"; "2. Intermediate"; "3. Experienced" ]
    (get_blank 280 20)

let q4 () =
  let question =
    Widget.text_display "4.  How would you describe your risk tolerance?"
  in
  radio_question question
    [
      "1. Conservative (low risk, stable returns)";
      "2. Moderate (balanced risk and return)";
      "3. Aggressive (high risk, higher potential returns)";
    ]
    (get_blank 70 20)

let q5 () =
  let question =
    Widget.text_display "5.  What is your investment time horizon?"
  in
  radio_question question
    [
      "1. Short-term (1-3 years)";
      "2. Medium-term (3-7 years)";
      "3. Long-term (7+ years)";
    ]
    (get_blank 210 20)

let q6 () =
  let question =
    Widget.text_display
      "6.  How many stocks would you like for diversification?"
  in
  radio_question question
    [
      "1. Small (3-5 stocks)";
      "2. Medium (5-10 stocks)";
      "3. Large (10+ stocks)";
    ]
    (get_blank 220 20)

let q7 () =
  let question_layout =
    set_layout
      (Widget.text_display
         "7.  Are you willing to invest in assets that may temporarily lose \
          money?")
      20
  in
  let radiolist, row_layout = vertical_radiolist [ "Yes"; "No" ] in
  let answer = Layout.flat [ row_layout; get_blank 345 20 ] in
  let rounded =
    rounded_panel 700 130 10
      (Layout.tower ~align:Draw.Center ~hmargin:172 [ question_layout; answer ])
  in
  let full_layout =
    Layout.flat ~align:Draw.Center [ get_blank 190 20; rounded ]
  in
  (full_layout, radiolist)

let q8 () =
  let question_layout =
    set_layout (Widget.text_display "8.  Do you currently have investments?") 20
  in
  let radiolist, row_layout = vertical_radiolist [ "Yes"; "No" ] in
  let answer = Layout.flat [ row_layout; get_blank 345 20 ] in
  let rounded =
    rounded_panel 700 130 10
      (Layout.tower ~align:Draw.Center ~hmargin:172 [ question_layout; answer ])
  in
  let full_layout = Layout.flat ~align:Draw.Min [ get_blank 190 20; rounded ] in
  (full_layout, radiolist)

let q9 () =
  let question_layout =
    set_layout
      (Widget.text_display
         "9.  If yes, please list your current investments.\n\
          (ticker symbols, separated by commas)")
      20
  in
  let answer = Widget.text_input ~max_size:1000 () in
  let answer_layout =
    Layout.flat [ get_blank 1 20; Layout.resident ~w:200 answer ]
  in
  let rounded =
    rounded_panel 700 110 10
      (Layout.tower ~align:Draw.Min ~hmargin:20
         [ question_layout; answer_layout ])
  in
  let full_layout =
    Layout.flat ~align:Draw.Center [ get_blank 260 20; rounded ]
  in
  (full_layout, answer)

let submit () =
  let submit =
    Widget.button ~border_radius:10 ~label:(Label.create ~size:15 "Submit") ""
  in
  ( submit,
    Layout.flat ~align:Draw.Center
      [ Layout.resident ~w:900 ~h:40 (Widget.label ""); Layout.resident submit ]
  )

let check_investments curr =
  let investments =
    curr |> String.split_on_char ',' |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  investments = []

let check_filled name radios inves curr =
  Widget.get_text name <> ""
  && List.for_all (fun r -> Radiolist.get_index r <> None) radios
  && not
       (Radiolist.get_index inves = Some 0
       && Widget.get_text curr = ""
       && check_investments (Widget.get_text curr))

let show_missing_popup layout =
  Popup.info ~w:200 ~h:50 "MISSING ANSWERS\nPlease answer all questions." layout

let set_answers name goal exp risk horizon size willing inves curr =
  let get_value x = string_of_int (Option.get (Radiolist.get_index x) + 1) in
  let investments =
    if Radiolist.get_index inves = Some 1 then "" else Widget.get_text curr
  in
  quiz_answers :=
    [
      Widget.get_text name;
      get_value goal;
      get_value exp;
      get_value risk;
      get_value horizon;
      get_value size;
      get_value willing;
      get_value inves;
      investments;
    ]

let build_heading () =
  let heading =
    rounded_panel 700 80 10
      (Layout.flat ~align:Draw.Min
         [
           get_blank 230 20;
           Layout.resident
             (Widget.label ~size:30 "   Please answer the following questions:");
           get_blank 330 20;
         ])
  in
  Layout.flat ~align:Draw.Center [ get_blank 37 20; heading ]

let build_question_content name goal exp risk hor size will inves curr submit =
  let header =
    rounded_panel
      ~bg_color:(Style.Solid Draw.(opaque (164, 211, 222)))
      ~border_color:Draw.(opaque (164, 211, 222))
      700 13 10 (get_blank 700 10)
  in
  let header_layout =
    Layout.flat ~align:Draw.Center [ get_blank 258 5; header ]
  in
  Layout.tower
    [
      get_blank 700 20;
      header_layout;
      build_heading ();
      name;
      goal;
      exp;
      risk;
      hor;
      size;
      will;
      inves;
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
  let inves, q8 = q8 () in
  let curr, q9 = q9 () in
  let submit, submit_layout = submit () in
  let content =
    build_question_content name goal exp risk hor size will inves curr
      submit_layout
  in
  let layout = Layout.make_clip ~h:500 content in
  Space.full_width layout;
  Space.full_height layout;
  let submit_form _widget =
    if not (check_filled input [ q2; q3; q4; q5; q6; q7 ] q8 q9) then
      show_missing_popup layout
    else (
      set_answers input q2 q3 q4 q5 q6 q7 q8 q9;
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
  let title = Widget.label ~size:50 "=== Portfolio Optimizer Quiz ===" in
  let button_label = Label.create ~size:32 "START" in
  let button = Widget.button ~border_radius:25 ~label:button_label "" in
  let welcome =
    Widget.label ~size:32 "Welcome! Let's find the perfect portfolio for you."
  in
  let instructions = Widget.label ~size:32 "Click [START] to begin." in
  let title_layout = Layout.flat_of_w ~align:Draw.Center [ title ] in
  let welcome_layout = Layout.flat_of_w ~align:Draw.Center [ welcome ] in
  let instructions_layout =
    Layout.flat_of_w ~align:Draw.Center [ instructions ]
  in
  let text_layout =
    rounded_panel 1175 200 0
      (Layout.tower ~align:Draw.Center
         [ title_layout; welcome_layout; instructions_layout ])
  in
  let content = build_page_content text_layout 150 26 30 50 in
  let layout =
    Layout.tower ~sep:(-11) ~align:Draw.Center
      [ content; make_button button 85 ]
  in
  Space.full_width layout;
  Space.full_height layout;
  let switch_screens _widget = Layout.set_rooms layout [ next_page ] in
  Widget.on_button_release ~release:switch_screens button;
  Layout.flat ~hmargin:(-10)
    ~background:(Layout.color_bg Draw.(211, 228, 232, 500))
    [ layout ]

let endpage prev_page =
  let title = Widget.label ~size:50 "Thank you!" in
  let heading = Widget.label ~size:40 "Your responses have been recorded." in
  let title_layout = Layout.flat_of_w ~align:Draw.Center [ title ] in
  let heading_layout = Layout.flat_of_w ~align:Draw.Center [ heading ] in
  let text_layout =
    rounded_panel 1175 200 0
      (Layout.tower ~align:Draw.Center [ title_layout; heading_layout ])
  in
  let content = build_page_content text_layout 150 36 0 25 in
  let button_label = Label.create ~size:32 "EXIT" in
  let button = Widget.button ~border_radius:25 ~label:button_label "" in
  let layout =
    Layout.tower ~sep:(-11) ~align:Draw.Center
      [ content; make_button button 75 ]
  in
  Space.full_width layout;
  Space.full_height layout;
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
