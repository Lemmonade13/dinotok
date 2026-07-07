let _ = Lwt_main.run (Dinotok.run_dino ())

(* let rec after_dino () = let%lwt () = Lwt_io.printl "Select mode: (1) Online
   (2) Offline" in let%lwt choice = Lwt_io.read_line Lwt_io.stdin in let
   online_mode = choice = "1" in

   if online_mode then start_session () (* run once *) else let%lwt () =
   start_dino () >>= fun () -> Lwt_io.printl "test" in after_dino () (* run
   again *) in

   after_dino () *)
