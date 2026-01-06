type 'a solution = Nil | Notin of 'a | Wrong of ('a * int) | Correct of ('a * int);;
type language = FR | EN;;
type type_guess = ENTROPY | RANDOM;;

let print_solution = function
  | Notin c -> Printf.printf "Notin(%c) " c
  | Wrong (c, i) -> Printf.printf "Wrong (%c,%i) " c i
  | Correct (c, i) -> Printf.printf "Correct (%c, %i) " c i
  | Nil -> Printf.printf "Nil ";
;;

let print_list_solution l =
  Printf.printf "[";
  List.iter print_solution l;
  Printf.printf "]";
  print_newline ();
;;

let read_dict path =
  let ic = open_in path in
  let l = ref [] in
  try
    while true do
      let line = 
        input_line ic
        |> String.trim
      in
      l := line :: !l;
    done;
  with
    End_of_file -> (
      close_in ic;
      !l
    )
;;

let make_corresp s guess =
  let s = s |> String.split_on_char ' ' in
  let rec aux i acc = function
    | [] -> List.rev acc
    | hd::tl -> (
        let curr = guess.[i] in
        let to_add =
          match hd with
            | "n" -> Notin curr
            | "w" -> Wrong (curr, i)
            | "c" -> Correct (curr, i)
            | _ -> failwith "error make_corresp : pas les bonnes conventions."
        in
        aux (i+1) (to_add::acc) tl
    )
  in
  aux 0 [] s
;;

let auto_corresp word guess =
  if String.length word <> String.length guess then
    invalid_arg "auto_corresp: length mismatch";

  let n = String.length guess in
  let sol = Array.make n Nil in

  for i = 0 to n - 1 do
    let w = word.[i] in
    let g = guess.[i] in
    sol.(i) <-
      if g = w then
        Correct (g, i)
      else if String.exists ((=) g) word then
        Wrong (g, i)
      else
        Notin g
  done;

  Array.to_list sol
;;

let entropy length_dict cnt_groups =
  let n = float_of_int length_dict in
  let sum = 
    Array.fold_left (fun r e ->
      if e = 0
      then 0. +. r
      else
        let p_p = (float_of_int e) /. n in
        (p_p *. Float.log2 p_p) +. r
    ) 0. cnt_groups
  in
  -. sum
;;

(* Function that convert to bases 3 *)
let convert sol =
  let env = function
    | Notin _ -> 0
    | Wrong _ -> 1
    | Correct _ -> 2
    | _ -> failwith "error entropy env"
  in
  let digits = List.map env sol in
  List.fold_left (fun acc d -> acc * 3 + d) 0 digits
;;

let guess_entropy length_dict guess dict =
  let groups = Array.make 243 0 in
  List.iter (fun word ->
    let sol = auto_corresp word guess in
    let id = convert sol in
    groups.(id) <- groups.(id) + 1;
  ) dict;
  entropy length_dict groups
;;

let max_entropy ?(debug=false) dict =
  let len = List.length dict in
  let entropys = List.mapi (fun i g -> (i, guess_entropy len g dict)) dict in
  let pos = List.fold_left (fun r en -> if snd en > snd r then en else r) (0, 0.) entropys in
  let w = List.nth dict (fst pos) in
  if debug then Printf.printf "Choose word : %s with entropy = %f\n" w (snd pos);
  w
;;

let make_guess ?(debug=false) tg dict =
  Random.self_init ();
  match dict with
    | [] -> failwith "make_guess: dictionnaire vide"
    | [x] -> x
    | _ ->
      match tg with
        | ENTROPY -> max_entropy ~debug:debug dict
        | RANDOM -> (
          let n = List.length dict in
          let i = Random.int n in
          List.nth dict i
        )
;;

(**
  Mot optimal pour avoir un point de départ qui va tendre plus rapidement vers la bonne solution
**)
let make_first_guess lang =
  Random.self_init ();
  let n = 5 in
  let i = Random.int_in_range ~min:0 ~max:(n-1) in
  let opt =
    match lang with
      | FR -> [|"raies"; "adieu"; "orner"; "saine"; "toile"|]
      | EN -> [|"soare"; "roate"; "raise"; "arise"; "slate"|] 
  in
  opt.(i)
;;

let condition_arret =
  List.for_all (fun s -> match s with Correct (_) -> true | _ -> false)
;;

let propagation sol dict =
  let present =
    sol
    |> List.filter_map (function
        | Correct (c, _) | Wrong (c, _) -> Some c
        | _ -> None)
  in

  let rec aux acc = function
    | [] -> acc
    | hd::tl ->
        let new_acc =
          match hd with
          | Notin c ->
              if List.mem c present then acc
              else List.filter (String.for_all ((<>) c)) acc
          | Wrong (c, i) ->
              List.filter
                (fun str -> str.[i] <> c && String.exists ((=) c) str)
                acc
          | Correct (c, i) ->
              List.filter (fun str -> str.[i] = c) acc
          | Nil -> failwith "error propagation : Nil"
        in
        aux new_acc tl
  in
  aux dict sol
;;

let solve ?(beg=None) methd path =
  let debug =
    match methd with
      | ENTROPY -> true
      | _ -> false
  in
  let lang =
    match path with
      | "files/words.txt" -> EN
      | _ -> FR
  in
  let dict = ref (read_dict path |> List.filter (fun s -> String.length s = 5)) in
  let tf = ref true in
  let guess =
    match beg with
      | None -> ref (make_first_guess lang) 
      | Some s -> ref s
  in

  while !tf do
    if (List.length !dict) = 1
    then (guess := List.hd !dict; tf := false)
    else (
      Printf.printf "guess : %s\n" !guess;
      Printf.printf "Usage for each char separated by spaces : n -> not in word ; w -> wrong spot ; c -> correct spot\n";
      print_endline "your turn : ";
      print_newline ();
      let ans = read_line () in

      let solution = make_corresp ans !guess in
      print_list_solution solution;

      if condition_arret solution
      then tf := false
      else (dict := propagation solution !dict; guess := make_guess ~debug:debug methd !dict; )
    )
  done;

  Printf.printf "The word is : %s\n" !guess;