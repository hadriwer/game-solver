exception Fin;;

let () =
  try
    while true do
      Printer.choose_solver ();
      Printf.printf "Make a choice of solver to execute : ";
      let choice = read_int () in
      Printer.sep '-' 30;
      match choice with
        | 0 -> (print_endline "quit"; raise Fin)
        | 1 -> (Sudoku_solver.solver "files/sudoku.txt"; raise Fin)
        | 2 -> (
          Printer.choose_lang ();
          let lang = 
            match read_int () with
              | 1 -> "files/words.txt"
              | _ -> "files/mots.txt"
          in
          Wordle_solver.solve ~beg:(None) Wordle_solver.ENTROPY lang;
          raise Fin
        )
        | 3 -> (Fubuki_solver.solve "files/fubuki.txt"; raise Fin)
        | 4 -> (Crosswords_solver.solve "files/crosswords.txt"; raise Fin)
        | 5 -> (Cemantix_solver.solve (); raise Fin)
        | _ -> print_endline "unknown Parameter";
    done;
  with Fin -> Printf.printf "End of the solver.\n";
