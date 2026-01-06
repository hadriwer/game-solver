
let () =
  let path = "files/crosswords.txt" in
  (* Sudoku_solver.solver path;
  print_endline "Hello, World!" *)

  (* stats ENTROPY ~maxdpl:1000 path *)
  (* Wordle_solver.solve ~beg:(None) Wordle_solver.ENTROPY path *)

  (* Fubuki_solver.solve path; *)

  Crosswords_solver.solve path;