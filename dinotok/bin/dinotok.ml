open Lwt.Infix

let () = Random.self_init ()
let init_distance = 50

(* positions for character: pos1 is the default, pos2-pos8 are for flips*)

let pos1 = [ " O "; "/|\\"; "/ \\" ]
let pos2 = [ "O_"; "|\\_"; "  \\" ]
let pos3 = [ "O/__/"; " \\  \\" ]
let pos4 = [ " |_ "; "|/_"; "O  " ]
let pos5 = [ "\\ / "; "\\|/ "; " O " ]
let pos6 = [ " _|"; " _\\|"; "   O" ]
let pos7 = [ "\\__\\"; "/  /O" ]
let pos8 = [ "  _O"; " _/|"; "  |" ]
let slide = [ " O_"; " _\\" ]
let white_on_black = "\027[37;40m"
let reset = "\027[0m"

(* decreases over time *)
let fps = ref 0.051
let input_flag = ref false

type action =
  | STAND
  | JUMP
  | FRONTFLIP
  | BACKFLIP
  | SLIDE

type input = {
  mutable until_input : int;
  mutable move : action;
}

let input_data = { until_input = 0; move = STAND }

(* make array with record containing info about obstacles so maybe in air *)
type obstacle = {
  mutable loc : int;
  height : int;
}

type plane = {
  ascii : string list;
  mutable pos : int;
  mutable send : bool;
  mutable charge : int;
}

let f16_ascii =
  [
    "  ___                                   ";
    " |   \\                                  ";
    " |    \\                   ___           ";
    " |_____\\______________.-'`   `'-.,___   ";
    "/| _____     _________            ___>---";
    "\\|___________________________,.-'`       ";
    "          `'-.,______________)           ";
  ]

let bomb =
  [ " 6767  676767"; "67        67"; "67676    67"; "67  67  67"; " 6767  67" ]

(* 6 + 3 below plane; 9 total frames *)
let screen_w = 56
let plane_w = List.fold_left (fun m s -> max m (String.length s)) 0 f16_ascii
let f16 = { ascii = f16_ascii; pos = -plane_w; send = false; charge = 200 }

let clip_right s =
  if String.length s <= screen_w then s else String.sub s 0 screen_w

let pad_left dx s = if dx <= 0 then s else String.make dx ' ' ^ s

let f16_frame dx =
  if dx < 0 then
    f16.ascii
    |> List.map (fun line ->
           let len = String.length line in
           let k = len + dx in
           if k <= 0 then "" else clip_right (String.sub line (-dx) k))
    |> String.concat "\n"
  else
    f16.ascii
    |> List.map (fun line -> clip_right (pad_left dx line))
    |> String.concat "\n"

let f16_string () =
  "\n"
  ^
  if f16.send then "\n" ^ f16_frame f16.pos
  else
    (if f16.charge > 300 then "Press F to send an F-16" else "")
    ^ String.make 7 '\n'

let score = ref 0
let max_obs = 10
let obstacles = Array.init max_obs (fun _ -> { loc = -8; height = 0 })
let obs = ref 0
let drop = 0
let boom = 24
let place_low_obs = ref false
let place_high_obs = ref false

exception Petersen of string

let rec repeat s n = if n <= 0 then "" else s ^ repeat s (n - 1)

let boom_line num =
  if f16.pos > 17 then
    if f16.pos < 22 then
      if f16.pos > 17 - 5 + num then 7 + ((f16.pos - 18) * 4) else 0
    else if f16.pos < 32 then boom
    else if f16.pos < 40 - num then boom - ((f16.pos - 31) * 3)
    else 0
  else 0

let line = function
  | 7 | 1 -> 2
  | 6 | 2 -> 4
  | 5 | 3 -> 5
  | 4 -> 6
  | _ -> 0

let passed h =
  if obstacles.(0).height = h then
    match obstacles.(0).loc with
    | -7 -> "7     "
    | -1 -> "     6"
    | x when x > -7 && x < -1 ->
        String.make (6 + x) ' ' ^ "67" ^ String.make (-2 - x) ' '
    | _ -> String.make 6 ' '
  else String.make 6 ' '

let behind h =
  if obstacles.(0).height = h then
    match obstacles.(0).loc with
    | -7 -> "7  "
    | -4 -> "  6"
    | -5 -> " 67"
    | -6 -> "67 "
    | _ -> String.make 3 ' '
  else String.make 3 ' '

let move_obs () =
  for i = 0 to min !obs max_obs - 1 do
    obstacles.(i).loc <- obstacles.(i).loc - 1
  done

let rec search j h =
  if j < 0 then None
  else if obstacles.(j).height = h && obstacles.(j).loc >= 0 then Some j
  else search (j - 1) h

let obs_string h =
  let obs_line = ref "" in
  for i = 0 to !obs - 1 do
    if obstacles.(i).height = h && obstacles.(i).loc > -2 then begin
      let prev = search (i - 1) h in
      let spaces =
        match prev with
        | None -> obstacles.(i).loc
        | Some j -> obstacles.(i).loc - obstacles.(j).loc - 2
      in
      obs_line :=
        !obs_line
        ^ String.make (max 0 spaces) ' '
        ^
        if obstacles.(i).loc = -1 then "7"
        else if obstacles.(i).loc = init_distance - 1 then "6"
        else "67"
    end
  done;
  !obs_line

let join_lines lines =
  let h = line input_data.until_input in
  match lines with
  | [ l1; l2; l3 ] ->
      l1
      ^ obs_string (h + 2)
      ^ "\n"
      ^ behind (h + 1)
      ^ l2 ^ "\n" ^ behind h ^ l3
  | [ l1; l2 ] -> l1 ^ "\n" ^ behind h ^ l2
  | _ -> invalid_arg "join_lines expects 2 or 3 strings"

let stand_high lines =
  match lines with
  | [ l1; l2; l3 ] ->
      l1 ^ obs_string 2 ^ "\n" ^ "   " ^ l2 ^ "\n" ^ behind 0 ^ l3
  | _ -> invalid_arg "join_lines expects exactly 3 strings"

let line_0 pos =
  if pos = slide then passed 2 ^ obs_string 2 ^ "\n" ^ "   " ^ join_lines pos
  else behind 2 ^ stand_high pos

let bomb_line line =
  let n = 12 - line + ((drop - f16.pos) / 2) in
  if n < 0 || n > 4 then "" else List.nth bomb n

let end_obs len line s c =
  let b = bomb_line line in
  b
  ^
  let start = c + String.length b in
  if start >= len then "" else String.sub s start (len - start)

let join_bomb pos =
  let h = line input_data.until_input in
  let base = if f16.pos - drop > 9 then drop + 25 else f16.pos + 15 in
  match pos with
  | [ l1; l2; l3 ] ->
      behind (h + 2)
      ^ l1
      ^ (if h = 0 then
           let x = obs_string 2 in
           if f16.pos - drop > 11 then
             let n = String.length x in
             let c = base - String.length l1 in
             if n > c then String.sub x 0 c else x ^ String.make (c - n) ' '
           else x
         else String.make (base - String.length l1) ' ')
      ^ bomb_line (h + 2)
      ^ "\n" ^ "   " ^ l2
      ^ String.make (base - String.length l2) ' '
      ^ bomb_line (h + 1)
      ^ "\n" ^ behind h ^ l3
      ^
      if h = 2 then
        let x = obs_string 2 in
        if f16.pos - drop > 11 then
          let n = String.length x in
          let c = base - String.length l3 in
          (if n > c then String.sub x 0 c else x ^ String.make (c - n) ' ')
          ^ end_obs n 2 x c ^ "\n"
        else x
      else if h = 0 then
        let x = obs_string 0 in
        if f16.pos - drop > 11 then
          let n = String.length x in
          let c = base - String.length l3 in
          (if n > c then String.sub x 0 c else x ^ String.make (c - n) ' ')
          ^ end_obs n 0 x c
        else x
      else String.make (base - String.length l3) ' ' ^ bomb_line h ^ "\n"
  | [ l1; l2 ] ->
      (if h = 0 then
         passed 2
         ^
         let x = obs_string 2 in
         if f16.pos - drop > 11 then
           let n = String.length x in
           let c = base - 3 in
           if n > c then String.sub x 0 c else x ^ String.make (c - n) ' '
         else x
       else String.make (base + 3) ' ')
      ^ bomb_line (h + 2)
      ^ "\n"
      ^ behind (h + 1)
      ^ l1
      ^ String.make (base - String.length l1) ' '
      ^ bomb_line (h + 1)
      ^ "\n" ^ behind h ^ l2
      ^
      if h = 0 then
        let x = obs_string 0 in
        if f16.pos - drop > 13 then
          let n = String.length x in
          let c = base - String.length l2 in
          (if n > c then String.sub x 0 c else x ^ String.make (c - n) ' ')
          ^ end_obs n 0 x c
        else x
      else String.make (base - String.length l2) ' ' ^ bomb_line h ^ "\n"
  | _ -> invalid_arg "join_lines expects 2 or 3 strings"

let bomb_string line pos =
  let diff = f16.pos - drop in
  let base = if diff > 9 then drop + 28 else f16.pos + 18 in
  "\n"
  ^
  if line = 0 then
    (let x = ref "" in
     for i = 8 downto 3 do
       x := !x ^ String.make base ' ' ^ bomb_line i ^ "\n"
     done;
     !x)
    ^ if diff < 12 then line_0 pos ^ obs_string 0 else join_bomb pos
  else if line = 2 then
    (let x = ref "" in
     for i = 8 downto 2 + List.length pos do
       x := !x ^ String.make base ' ' ^ bomb_line i ^ "\n"
     done;
     !x)
    ^ join_bomb pos
    ^
    let c = drop + 22 in
    if diff > 11 then
      String.make base ' ' ^ bomb_line 1 ^ "\n" ^ passed 0
      ^
      let y = obs_string 0 in
      let m = String.length y in
      (if m > c then String.sub y 0 c else y ^ String.make (c - m) ' ')
      ^ end_obs m 0 y c
    else "\n\n" ^ passed 0 ^ obs_string 0
  else
    let x = ref "" in
    for i = 8 downto line + 3 do
      x := !x ^ String.make base ' ' ^ bomb_line i ^ "\n"
    done;
    !x ^ join_bomb pos
    ^
    let y = ref "" in
    for i = line - 1 downto 3 do
      y := !y ^ String.make base ' ' ^ bomb_line i ^ "\n"
    done;
    !y ^ passed 2
    ^
    let z = obs_string 2 in
    if diff > 11 then
      let n = String.length z in
      let c = drop + 22 in
      (if n > c then String.sub z 0 c else z ^ String.make (c - n) ' ')
      ^ end_obs n 2 z c ^ "\n"
      ^ String.make (drop + 28) ' '
      ^ bomb_line 1 ^ "\n" ^ passed 0
      ^
      let y = obs_string 0 in
      let m = String.length y in
      (if m > c then String.sub y 0 c else y ^ String.make (c - m) ' ')
      ^ end_obs m 0 y c
    else z ^ "\n\n" ^ passed 0 ^ obs_string 0

let join_boom lines =
  let h = line input_data.until_input in
  let bond s line =
    let num = boom_line line in
    let base = 28 - num - String.length s in
    behind line ^ s
    ^ (if base > 0 then String.make base ' ' else "")
    ^ repeat "67" num
    ^ if line > 0 then "\n" else ""
  in
  match lines with
  | [ l1; l2; l3 ] -> bond l1 (h + 2) ^ bond l2 (h + 1) ^ bond l3 h
  | [ l1; l2 ] -> bond "" (h + 2) ^ bond l1 (h + 1) ^ bond l2 h
  | _ -> invalid_arg "join_lines expects 2 or 3 strings"

let explosion line pos =
  "\n"
  ^
  let x = ref "" in
  for i = 8 downto line + 3 do
    let n = boom_line i in
    x := !x ^ String.make (31 - n) ' ' ^ repeat "67" n ^ "\n"
  done;
  !x ^ join_boom pos
  ^
  let y = ref "" in
  for i = line - 1 downto 0 do
    let n = boom_line i in
    y := !y ^ String.make (31 - n) ' ' ^ repeat "67" n ^ "\n"
  done;
  !y

let output line pos =
  let third_line = if pos = pos3 || pos = pos7 || pos = slide then 1 else 0 in
  let no_high = search 9 2 = None in
  String.make 20 '\n' ^ "Score: " ^ string_of_int !score ^ f16_string ()
  ^
  if f16.pos >= drop && f16.pos < drop + 18 then bomb_string line pos
  else if f16.pos >= drop + 18 && f16.pos < 40 then explosion line pos
  else if no_high || line >= 2 then
    (if pos = slide then
       String.make (7 - line) '\n' ^ passed 2 ^ obs_string 2 ^ "\n"
     else String.make (7 - line + third_line) '\n')
    ^ behind (line + List.length pos - 1)
    ^ join_lines pos
    ^ (if no_high && line < 2 then String.make line '\n'
       else
         String.make (line - 2) '\n'
         ^ (if line = 2 then "" else passed 2)
         ^ obs_string 2 ^ String.make 2 '\n')
    ^ if line = 0 then "" else passed 0
  else String.make 7 '\n' ^ line_0 pos

let jump n =
  let line = line n in
  output line pos1

let slide () = output 0 slide

let backflip n =
  let line = line n in
  output line
    (match n with
    | 7 -> pos2
    | 6 -> pos3
    | 5 -> pos4
    | 4 -> pos5
    | 3 -> pos6
    | 2 -> pos7
    | 1 -> pos8
    | _ -> pos1)

let frontflip n =
  let line = line n in
  output line
    (match n with
    | 7 -> pos8
    | 6 -> pos7
    | 5 -> pos6
    | 4 -> pos5
    | 3 -> pos4
    | 2 -> pos3
    | 1 -> pos2
    | _ -> pos1)

let wait_for_quiet () =
  let rec loop () =
    let ready, _, _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
    if ready = [] then Lwt.return_unit
    else Lwt_io.read_char Lwt_io.stdin >>= fun _ -> Lwt_unix.sleep !fps >>= loop
  in
  loop ()

let in_press = ref false

(* Terminal mode setup/restore *)
let original_termios = Unix.tcgetattr Unix.stdin

let enable_raw_mode () =
  let raw = { original_termios with Unix.c_icanon = false; c_echo = false } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW raw

let restore_terminal () =
  Unix.tcsetattr Unix.stdin Unix.TCSANOW original_termios

let can_place () = f16.pos < 22 && ((!obs = 0 && float_of_int (!score + 50) *. !fps > 4.5)
           || !obs > 0
              && obstacles.(!obs - 1).loc < init_distance - 13)

let rec print_loop controller_oc =
  Lwt.catch
    (fun () ->
      let had_input = !input_flag in
      input_flag := false;

      if !fps > 0.03 && !score mod 100 = 0 then fps := !fps -. 0.001 else ();
      if f16.pos = 18 then (
        obs := 0;
        for i = 0 to max_obs - 1 do
          obstacles.(i) <- { loc = -8; height = 0 }
        done)
      else if f16.pos > 18 then ()
      else if obstacles.(0).loc = -7 then (
        obs := !obs - 1;
        for i = 0 to !obs do
          obstacles.(i) <-
            { (obstacles.(i + 1)) with loc = obstacles.(i + 1).loc - 1 }
        done)
      else move_obs ();
      let new_67 =
        f16.pos < 22
        && ((!obs = 0 && float_of_int (!score + 50) *. !fps > 4.5)
           || !obs > 0
              && obstacles.(!obs - 1).loc < init_distance - 13
              && Random.int 10 = 1)
      in
      if !place_low_obs && !obs < max_obs then (
        obstacles.(!obs) <- { loc = init_distance - 1; height = 0 };
        obs := !obs + 1;
        place_low_obs := false)
      else if !place_high_obs && !obs < max_obs then (
        obstacles.(!obs) <- { loc = init_distance - 1; height = 2 };
        obs := !obs + 1;
        place_high_obs := false)
      else if new_67 then (
        match controller_oc with
        | Some _ -> ()
        | None -> 
        let level =
          match Random.int 3 with
          | 2 -> 2
          | _ -> 0
        in
        obstacles.(!obs) <- { loc = init_distance - 1; height = level };
        obs := !obs + 1)
      else ();
      score := !score + 1;
      if f16.send then
        if f16.pos = screen_w then (
          f16.send <- false;
          f16.pos <- -plane_w;
          f16.charge <- 0)
        else f16.pos <- f16.pos + 1
      else f16.charge <- f16.charge + 1;
      let led_signal =
        match controller_oc with
        | Some oc ->
          (* this code doesn't actually work but thats ok *)
          let c = if can_place () then '1' else '0' in
          Lwt_io.write_char oc c
          >>= fun () ->
          Lwt_io.flush oc
        | None -> Lwt.return_unit
      in
      let msg =
        (match input_data.move with
        | STAND -> jump input_data.until_input
        | JUMP -> jump input_data.until_input
        | FRONTFLIP -> frontflip input_data.until_input
        | BACKFLIP -> backflip input_data.until_input
        | SLIDE -> slide ())
        ^ if f16.pos >= drop then "" else obs_string 0
      in
      if (not had_input) && input_data.until_input = 0 then ()
      else if not had_input then
        input_data.until_input <- input_data.until_input - 1
      else input_data.until_input <- 7;
      let output =
        (if !score mod 1000 > 500 then white_on_black else "") ^ msg ^ reset
      in
      led_signal >>= fun () ->
      Lwt_io.printl output >>= fun () ->
      Lwt_unix.sleep !fps >>= fun () ->
      let hit_high =
        match input_data.move with
        | STAND -> true
        | JUMP | FRONTFLIP | BACKFLIP ->
            line input_data.until_input = 2 || line input_data.until_input = 0
        | SLIDE -> false
      in
      if
        obstacles.(0).loc = 0
        && ((obstacles.(0).height = 0 && input_data.until_input = 0)
           || (obstacles.(0).height = 2 && hit_high))
      then raise (Petersen "Petersen is coming for you");
      print_loop controller_oc)
    (function
      | Petersen msg ->
          Lwt_unix.sleep 1.8 >>= fun () ->
          restore_terminal ();
          print_endline msg;
          let%lwt () = Lwt_unix.sleep 1.8 in
          Lwt.return_unit
      | e ->
          restore_terminal ();
          Lwt.fail e)

(* --- Input loop with cooldown + raw single-character reads --- *)

let cooldown = ref Lwt.return_unit (* resolves when input is allowed *)

let rec input_loop device serial_ic =
  let do_jump () =
    input_flag := true;
    cooldown := Lwt_unix.sleep (7.0 *. !fps);
    input_loop device serial_ic
  in
  (* Wait for cooldown to finish *)
  !cooldown >>= fun () ->
  (* Wait until keys are quiet *)
  wait_for_quiet () >>= fun () ->
  (* Read next input *)
  (if serial_ic == Lwt_io.stdin then Lwt_io.read_char_opt Lwt_io.stdin
   else
     Lwt.pick
       [ Lwt_io.read_char_opt Lwt_io.stdin; Lwt_io.read_char_opt serial_ic ])
  >>= function
  (* ENTER: same behavior as before *)
  | Some '\n' | Some '\r' ->
      input_data.move <- JUMP;
      do_jump ()
  | Some 'F' | Some 'f' ->
      if f16.charge > 300 then f16.send <- true else ();
      (* should be 300, 200 at start*)
      input_loop device serial_ic
  | Some 'W' | Some 'w' ->
      input_data.move <- JUMP;
      do_jump ()
  | Some 'A' | Some 'a' ->
      input_data.move <- BACKFLIP;
      do_jump ()
  | Some 'S' | Some 's' ->
      if input_data.move = SLIDE then input_data.move <- STAND
      else if input_data.move = STAND || input_data.until_input = 0 then
        input_data.move <- SLIDE;
      input_loop device serial_ic
  | Some 'D' | Some 'd' ->
      input_data.move <- FRONTFLIP;
      do_jump ()
  (* ESC sequence (possible arrow key) *)
  | Some '\027' -> (
      (* Try to read '[' next *)
      Lwt_io.read_char_opt Lwt_io.stdin
      >>= function
      | Some '[' -> (
          (* Third char determines which arrow *)
          Lwt_io.read_char_opt Lwt_io.stdin
          >>= function
          | Some 'A' ->
              (* Up arrow: treat like Enter *)
              input_data.move <- JUMP;
              do_jump ()
          | Some 'B' ->
              (* down *)
              if input_data.move = SLIDE then input_data.move <- STAND
              else if input_data.move = STAND || input_data.until_input = 0 then
                input_data.move <- SLIDE;
              input_loop device serial_ic
          | Some 'C' ->
              (* right *)
              input_data.move <- FRONTFLIP;
              do_jump ()
          | Some 'D' ->
              (* left *)
              input_data.move <- BACKFLIP;
              do_jump ()
          | _ -> input_loop device serial_ic)
      | _ ->
          (* ESC not starting an arrow sequence — ignore *)
          input_loop device serial_ic
      (* Quit game *))
  | Some 'q' -> Lwt.return_unit
  (* All other keys: ignore, same as before *)
  | Some _ -> input_loop device serial_ic
  | None -> input_loop device serial_ic

let rec controller_loop serial_ic =
  Lwt_io.read_char_opt serial_ic >>= function
  | Some 'L' | Some 'l' ->
      if can_place () then place_low_obs := true;
      controller_loop serial_ic
  | Some 'H' | Some 'h' ->
      if can_place () then place_high_obs := true;
      controller_loop serial_ic
  | Some _ | None -> controller_loop serial_ic

let find_serial_devices () =
  (try
     let directory = Unix.opendir "/dev" in
     let rec loop acc =
       match Unix.readdir directory with
       | exception End_of_file ->
           Unix.closedir directory;
           acc
       | name
         when String.length name >= 10 && String.sub name 0 10 = "cu.usbmode" ->
           loop (("/dev/" ^ name) :: acc)
       | _ -> loop acc
     in
     loop []
   with _ -> [])
  |> function
  | [] ->
      let rec try_com n acc =
        if n > 20 then acc
        else
          let path = "\\\\.\\COM" ^ string_of_int n in
          match Unix.openfile path [ Unix.O_RDONLY; Unix.O_NOCTTY ] 0o600 with
          | _ -> try_com (n + 1) (path :: acc)
          | exception _ -> try_com (n + 1) acc
      in
      try_com 1 []
  | devices -> devices

let open_serial device =
  let fd = Unix.openfile device [ Unix.O_RDWR; Unix.O_NOCTTY ] 0o600 in
  let t = Unix.tcgetattr fd in
  let t' = { t with Unix.c_ibaud = 9600; c_obaud = 9600 } in
  Unix.tcsetattr fd Unix.TCSANOW t';
  let lwt_fd = Lwt_unix.of_unix_file_descr fd in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input lwt_fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output lwt_fd in
  (ic, oc)

let reset_game () =
  fps := 0.051;
  input_flag := false;
  input_data.until_input <- 0;
  input_data.move <- STAND;
  score := 0;
  obs := 0;
  place_low_obs := false;
  place_high_obs := false;
  for i = 0 to max_obs - 1 do
    obstacles.(i) <- { loc = -8; height = 0 }
  done;
  f16.pos <- -plane_w;
  f16.send <- false;
  f16.charge <- 200;
  cooldown := Lwt.return_unit

let choose_device devices prompt =
  let n = List.length devices in
  if n = 0 then begin
    print_endline (prompt ^ ": No boards found, using keyboard only.");
    None
  end else begin
    print_endline (prompt ^ ":");
    List.iteri (fun i d -> Printf.printf "  %d: %s\n" (i + 1) d) devices;
    Printf.printf "  0: No board (keyboard only)\n%!";
    let rec read_choice () =
      let buf = Bytes.create 1 in
      ignore (Unix.read Unix.stdin buf 0 1);
      let c = Char.code (Bytes.get buf 0) - Char.code '0' in
      if c = 0 then None
      else if c >= 1 && c <= n then Some (List.nth devices (c - 1))
      else begin
        print_endline "Invalid choice, try again.";
        read_choice ()
      end
    in
    read_choice ()
  end
(* --- Program entry with terminal mode setup/cleanup --- *)
let run_dino () : unit Lwt.t =
  reset_game ();
  enable_raw_mode ();
  let devices = find_serial_devices () in
  let player_device = choose_device devices "Player: choose a board (or use keyboard controls)" in
  let controller_device = choose_device devices "Controller: choose a board (or have random obstacles)" in
  let player_serial = Option.map open_serial player_device in
  let controller_serial = Option.map open_serial controller_device in
  let player_ic = Option.map fst player_serial in
  let controller_ic = Option.map fst controller_serial in
  let controller_oc = Option.map snd controller_serial in
  let p_ic = Option.value player_ic ~default:Lwt_io.stdin in
  Lwt.finalize
    (fun () ->
      let threads =
        [ print_loop controller_oc; input_loop "" p_ic ]
        @
        match controller_ic with
        | Some ic -> [ controller_loop ic ]
        | None -> []
      in
      Lwt.pick threads)
    (fun () ->
      restore_terminal ();
      Lwt.return_unit)
(* propagate to top-level catcher *)
