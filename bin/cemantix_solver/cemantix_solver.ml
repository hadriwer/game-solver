(* open Owl_nlp_tfidf *)

open Owl

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
      if !row mod 10000 = 0 then Printf.printf "Chargés : %d mots...\n%!" !row;
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

let solve () =
  let names, _ = read_instance "bin/cemantix_solver/wiki.fr.vec" in

  for i = 0 to 10 do
    Printf.printf "%s\n" names.(i);
  done;