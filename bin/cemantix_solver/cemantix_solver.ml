open Owl
let number_words = 80559.;;

type attempt = {
  idx : int;
  score : float;
};;

let read_instance filename =
  let ic = open_in filename in
  (* Lire l'en-tête pour connaître la dimension (ex: 300) *)
  let header = input_line ic |> String.split_on_char ' ' in
  let max_words = List.hd header |> int_of_string in
  let dim = List.nth header 1 |> int_of_string in
  
  (* On pré-alloue la matrice et le tableau de noms *)
  let names = Array.make max_words "" in
  let matrix = Dense.Matrix.S.empty max_words dim in

  let row = ref 0 in
  try
    while !row < max_words do
      let line = input_line ic in
      (* On trouve l'index du premier espace pour extraire le mot *)
      let first_space = String.index line ' ' in
      names.(!row) <- String.sub line 0 first_space;

      (* On parse les nombres manuellement ou via un helper *)
      let content = String.sub line (first_space + 1) (String.length line - first_space - 1) in
      let values = String.split_on_char ' ' content |> List.filter (fun s -> s <> "") in
      
      let col = ref 0 in
      List.iter (fun v ->
        if !col < dim then (
          Dense.Matrix.S.set matrix !row !col (float_of_string v);
          incr col
        )
      ) values;
      
      incr row;
      if !row mod 10000 = 0 
      then (
        Printf.printf "\027[A\027[2K\r %f %% words loaded...\n" ((!row |> float_of_int) /. number_words *. 100.);
        flush stdout
      )
    done;
    close_in ic;
    names, matrix
  with End_of_file ->
    close_in ic;
    (* On retaille si le fichier était plus petit que max_words *)
    let final_names = Array.sub names 0 !row in
    let final_matrix = Dense.Matrix.S.get_slice [[0; !row - 1]; []] matrix in
    final_names, final_matrix
;;

let normalize v =
  let inf = Owl_linalg.Generic.norm v in
  if inf = 0. then v
  else
    Owl_dense_matrix.Operator.(v /$ (Owl_linalg.Generic.norm v))
;;

let normalize_all rows cols m =
  print_endline "we normalise all the words";
  for i = 0 to rows - 1 do
    let v = Owl_dense_matrix.Generic.row m i in
    let nv = normalize v in
    for j = 0 to cols - 1 do
      Owl_dense_matrix.Generic.set m i j (Owl_dense_matrix.Generic.get nv 0 j);
    done;
  done
;;

let estimate_direction ?(alpha=2.) cols embeddings attempts =
  let v_dir = Dense.Matrix.S.zeros 1 cols in

  (* let mean_score =
    List.fold_left (fun acc a -> acc +. a.score) 0. attempts
    /. float_of_int (List.length attempts)
  in *)

  List.iter (fun a ->
    let v = Owl_dense_matrix.Generic.row embeddings a.idx in
    let w = (a.score /. 100.) ** alpha in
    let tmp = Dense.Matrix.S.(v *$ w) in
    Dense.Matrix.S.(v_dir += tmp);
  ) attempts;

  normalize v_dir
;;

let score_candidate v' v = 
  let s = Owl_dense_matrix.Generic.(get (dot v' (transpose v)) 0 0) in
  s ** 3.
;;

let find_best_neigh rows tbl m v =
  let best = ref (-1, neg_infinity) in

  for i = 0 to rows - 1 do
    if not (Hashtbl.mem tbl i) then (
      let v' = Owl_dense_matrix.Generic.row m i in
      let score = score_candidate v' v in
      if score > snd !best then
        best := (i, score)
    )
  done;
  let res = fst !best in
  if res = -1 then failwith "error find_best_voisin : find no close neigh."
  else res
;;

let add_list ?(taille=10) l elt =
  let rec aux e acc = function
    | [] -> e :: List.rev acc
    | hd::tl as xs -> if hd.score > e.score then (List.rev acc) @ (e :: xs) else aux e (hd::acc) tl
  in
  if List.length l = taille
  then (
    let hd = List.hd l in
    if hd.score > elt.score 
    then l
    else aux elt [] (List.tl l)
  ) else aux elt [] l
;;

let too_close ?(max_diff=10.) q =
  if Queue.is_empty q
  then false
  else
    let min_val = Queue.fold min max_float q in
    let max_val = Queue.fold max min_float q in
    (max_val -. min_val) <= max_diff
;;

let solve () =
  Random.self_init ();
  let names, embeddings = read_instance "bin/cemantix_solver/wiki.fr.filtered.vec" in
  let rows = Owl_dense_matrix.Generic.row_num embeddings in
  let cols = Owl_dense_matrix.Generic.col_num embeddings in
  normalize_all rows cols embeddings;

  let tf = ref true in
  let tbl = Hashtbl.create 1000 in
  let attemps = ref [] in
  let choice = ref (Random.int_in_range ~min:0 ~max:(rows-1)) in
  let guess = ref (names.(!choice)) in

  let latest_score = Queue.create () in
  let tour = ref 0 in

  while !tf do
    incr tour;
    Printf.printf "Guess by the solver : %s\n" !guess;
    Printf.printf "Cemantix output = ";

    let rec get_score () = try read_float () with Failure _ -> (Printf.printf "Input must be a float / int : "; get_score ()) in
    let score = get_score () in

    Hashtbl.add tbl !choice true;
    if score > 0. then (
      attemps := add_list !attemps {idx = !choice; score}
    );

    (* To escape from local minima *)
    Queue.add score latest_score;

    Printf.printf "Liste length = %d\n" (List.length !attemps);

    (if score = 1000.
    then tf := false
    else
      if List.length !attemps >= 1 && not (!tour mod 3 = 0 && (too_close latest_score)) then (
        print_endline "we estimate the direction";
        let v = estimate_direction cols embeddings !attemps in
        choice := find_best_neigh rows tbl embeddings v;
        Printf.printf "choice = %d\n" !choice;
      )
      else (
        print_endline "we choose a random word.";
        choice := Random.int_in_range ~min:0 ~max:(rows-1);
        if (!tour mod 3 = 0) then let _ = Queue.take latest_score in ()
      ));
    guess := names.(!choice);
    print_newline ();
  done;

  Printf.printf "Word was : %s in %d tours.\n" !guess !tour;