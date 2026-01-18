exception Fin;;

let () =
  try
    while true do
      Printer.choose_solver ();
      let choice = read_int () in
      match choice with
        | 0 -> (print_endline "quit"; raise Fin)
        | 1 -> Sudoku_solver.solver "files/sudoku.txt"
        | 2 -> (
          Printer.choose_lang ();
          let lang = 
            match read_int () with
              | 1 -> "files/words.txt"
              | _ -> "files/mots.txt"
          in
          Wordle_solver.solve ~beg:(None) Wordle_solver.ENTROPY lang
        )
        | 3 -> Fubuki_solver.solve "files/fubuki.txt"
        | 4 -> Crosswords_solver.solve "files/crosswords.txt"
        | 5 -> Cemantix_solver.solve ()
        | _ -> print_endline "unknown Parameter";
    done;
  with Fin -> Printf.printf "End of the solver.\n";
