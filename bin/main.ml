type direction_t = White | Black


type state = {
    position: int;
    direction: direction_t;
}

let generate () =
  let _ = Random.self_init() in
  Random.int(63) + 1
                  
let setup () =
  Raylib.init_window 800 800 "raylib [core] example - basic window";
  Raylib.set_target_fps 60;
  {  position = generate(); direction = White }

let draw_board position =
  let open Raylib in
  let size = 50 in
  let padding = 50 in
  let rec draw_row i j count =
    if j > 7 then
      draw_column (i + 1) 0 count
    else
      let color =
        if count = position then Color.red
        else if (j - i) mod 2 = 0 then Color.white
        else Color.darkgray
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
  match direction with
    | White -> draw_text "white" 0 0 20 Color.white
    | Black -> draw_text "black" 0 0 20 Color.darkgray

let toggle_direction direction =
  match direction with
  | White -> Black
  | Black -> White


let rec loop state =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    (* set_trace_log_level Debug; *)
    let mouse_clicked = is_mouse_button_pressed MouseButton.Left in
    let enter_pressed = is_key_pressed Key.Enter in
    let direction =
      if enter_pressed then
        toggle_direction (state.direction)   
      else state.direction in
    let position =
      if mouse_clicked then
        generate()
      else state.position in
    begin_drawing ();
        clear_background Color.lightgray;
        draw_text (string_of_int state.position) 500 200 200 Color.blue;
        draw_board (state.position);
        draw_direction (state.direction);
    end_drawing ();

    loop
      {
        direction;
        position;
      }

let () = setup () |> loop

