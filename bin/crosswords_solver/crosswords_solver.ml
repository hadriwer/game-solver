type fmt = (string * (int * int) list) list [@@deriving show];;

let get_pos_first str =
  ((str.[0] |> String.make 1 |> String.capitalize_ascii).[0]
  |> int_of_char) - 65
;;

let read_instance path =
  let ic = open_in path in
  let n = input_line ic |> int_of_string in
  let visited = Array.make 26 false in
  let words = ref [] in
  for _ = 0 to n do
    let line = input_line ic |> String.trim in
    let first = get_pos_first line in
    visited.(first) <- true;
    words := line :: !words;
  done;
  let x, y =
    let l =
      input_line ic
      |> String.split_on_char ' '
      |> List.map int_of_string
      |> Array.of_list
    in
    l.(0), l.(1)
  in
  let grid = Array.init y (fun _ ->
      ic |> input_line |> String.split_on_char ' ' |> List.filter ((<>) "")
      |> Array.of_list
  ) in
  close_in ic;
  n, words, grid, x, y, visited
;;

let match_suffix suffix =
  let len_suffix = String.length suffix in
  List.filter (fun word ->
    let s_word = String.sub word 0 (len_suffix) in
    suffix = s_word
  )
;;

let delete_word word =
  List.filter ((<>) word)
;;

let solve path =
  let _, w, grid, lenx, leny, visited = read_instance path in
  let print_crosswords coloration =
    Array.iteri (fun i row ->
      Array.iteri (fun j col ->
        if coloration.(i).(j)
        then Printf.printf "\027[32m%1s\027[0m " col
        else
          Printf.printf "%1s " col
      ) row;
      print_newline ();
    )
  in
  let in_bounds x y = 0 <= x && x < lenx && 0 <= y && y < leny in
  let direction x y dx dy =
    let rec aux x y acc_word acc = function      
      | l when in_bounds x y -> (
        let word = acc_word ^ (grid.(y).(x) |> String.lowercase_ascii) in
        match l with
          | [hd] as nw -> 
              (* we find the word so we delete in the words list*)
              if hd = acc_word 
              then (w := delete_word hd !w; (hd, acc))
              else 
                aux (x+dx) (y+dy) word ((x,y)::acc) nw
          | _ -> (
              let new_words = match_suffix word l in
              aux (x+dx) (y+dy) word ((x,y)::acc) new_words
          )
      )
      | [hd] -> 
          if hd = acc_word 
          then (w := delete_word hd !w; (hd, acc))
          else ("", [])
      | _ -> ("", [])
    in
    aux x y "" [] !w
  in
  let all_direction x y =
    [(1, 0); (1, 1); (1, -1); (0, 1); (0, -1); (-1, 0); (-1, 1); (-1, -1)]
    |> List.map (fun (dx, dy) -> direction x y dx dy)
    |> List.filter ((<>) ("", []))
  in
  let coloration = Array.make_matrix leny lenx false in
  let coord = ref [] in
  Array.iteri (fun i row ->
    Array.iteri (fun j _ ->
      let curr = grid.(i).(j) in
      let first = get_pos_first curr in
      if visited.(first) && !w <> []
      then (
        let res = all_direction j i in
        match res with
          | [] -> ()
          | _ -> print_endline (show_fmt res);
        let all_coord = List.fold_left (fun r (_,l) -> l @ r) [] res in
        coord := all_coord @ !coord
      )
    ) row
  ) grid;
  List.iter (fun (i,j) -> coloration.(j).(i) <- true) !coord;
  print_crosswords coloration grid;
;;
