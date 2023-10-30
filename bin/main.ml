type direction_t = White | Black

type state = {
    position: int;
    guess: int option;
    direction: direction_t;
    time: float option;
    score: int;
}

let generate () =
  let _ = Random.self_init() in
  Random.int(63) + 1

let cols = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]
let rows = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"]

let convert_position position direction =
  let open List in
  let is_last_column = position mod 8 == 0 in
  let line = (position / 8 ) - (if is_last_column then 1 else 0) in
  let col = if is_last_column then 7 else (position mod 8) -1 in
    match direction with
        | White -> Printf.sprintf "%s%s" (nth cols col) (nth (rev rows) line)
        | Black -> Printf.sprintf "%s%s" (nth (rev cols) col) (nth rows line)

let padding = 50
let size = 60 

let setup () =
  Raylib.init_window 800 600 "OCHESS";
  Raylib.set_target_fps 60;
  {  position = generate();
     direction = White;
     guess = None;
     time = None;
     score = 0;
  }

let draw_board position guess =
  let open Raylib in
  let rec draw_row i j count =
    if j > 7 then
      draw_column (i + 1) 0 count
    else
      let board_color = if (j - i) mod 2 = 0 then Color.white else Color.black in
      let color =
        match guess with
        | Some g -> if g == count then
                      if position == g then Color.green else Color.darkgray
                      else if position == count then Color.red else board_color
        | None -> board_color
      in
      let x_position = (padding + (size * j)) in
      let y_position =  (padding + (size * i)) in
      draw_rectangle x_position y_position size size color;
      draw_row i (j + 1) (count + 1)

  and draw_column i j count =
    if i > 7 then
      ()
    else
      draw_row i j count
  in
  draw_column 0 0 1

let draw_direction direction =
  let open Raylib in
  let y = size  * 9 in
  let x = padding in
  match direction with
  | White ->
      draw_text "white" x y 20 Color.white;
      draw_text "black" x 20 20 Color.black
  | Black ->
      draw_text "black" x y 20 Color.black;
      draw_text "white" x 20 20 Color.white

let toggle_direction direction =
  match direction with
  | White -> Black
  | Black -> White

let click_to_guess () =
  let open Raylib in
  let module V = Vector2 in
  let pos = get_mouse_position() in
  let min = padding in
  let max = padding + (size * 8) in
  let x = int_of_float(V.x pos) in
  let y = int_of_float(V.y pos) in
  if (x < min) || (x > max) || (y < min) || (y > max) then None
  else
  let x_cell = x / size in
  let y_cell = y / size in
  let pos = (x_cell + 8*(y_cell-1)) in
  Some pos

let draw_position_to_guess position direction =
  let open Raylib in
  let x = size *  10 in
  draw_text (convert_position position direction) x 200 80 Color.black

let draw_score score =
  let open Raylib in
  let x = size * 10 in
  draw_text (string_of_int score) x 350 50 Color.green

let rec loop state =
  let open Raylib in
  if window_should_close () then close_window ()
  else
    let draw () =
        begin_drawing ();
            clear_background Color.lightgray;
            draw_board state.position state.guess;
            draw_position_to_guess state.position state.direction;
            draw_score state.score;
            draw_direction (state.direction);
        end_drawing () in
    draw();
    match state.time with
        | Some t ->
            if (Unix.time() > t) then
              loop { state with time = None; guess = None; position = generate() }
            else loop state
        | None ->
            let mouse_clicked = is_mouse_button_pressed MouseButton.Left in
            let enter_pressed = is_key_pressed Key.Enter in
            let guess = if mouse_clicked then click_to_guess() else state.guess in
            let score = match guess with
              | Some p -> if p == state.position then (state.score + 1) else 0
              | None -> state.score
            in
            let time = match guess with
              | Some _ -> Some (Unix.time() +. 0.08)
              | None -> state.time
            in
            let direction =
            if enter_pressed then
                toggle_direction (state.direction) else state.direction in
            loop { state with direction; time; guess; score }


let () = setup () |> loop
