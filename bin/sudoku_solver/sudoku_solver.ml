type domain = Fixed of int | Domain of int list;;
type dir = Col| Lin;;

let line_valid dir fixed_idx ignore_idx e n grid =
  let tf = ref true in
  for i = 0 to n - 1 do
    if i <> ignore_idx then
      match dir with
      | Col -> tf := !tf && (grid.(i).(fixed_idx) <> e)
      | Lin -> tf := !tf && (grid.(fixed_idx).(i) <> e)
  done;
  !tf
;;

let is_square_valid x y e n grid =
  let len = sqrt (float_of_int n) |> int_of_float in
  let nx, ny = x - (x mod len), y - (y mod len) in
  let tf = ref true in
  for i = ny to ny + len - 1 do
    for j = nx to nx + len - 1 do
      if i <> y || j <> x
      then tf := !tf && (grid.(i).(j) <> e);
    done;
  done;
  !tf
;;

let is_valid_grid n grid =
  let tf = ref true in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      match grid.(i).(j) with
        | Domain _ -> ()
        | f -> tf := !tf && (line_valid Col j i f n grid) && (line_valid Lin i j f n grid) && (is_square_valid j i f n grid)
    done;
  done;
  !tf
;;

let init_domain n =
  Domain (List.init n (fun i -> i + 1))
;;

let read_instance path =
  let ic = open_in path in
  let n = input_line ic |> String.trim |> int_of_string in
  let res = Array.make n ([|Fixed 3|]) in
  for i = 0 to n - 1 do
    let line = 
      input_line ic 
      |> String.split_on_char ' ' 
      |> List.map (fun c -> 
          match c with 
            | "." -> init_domain n 
            | s -> Fixed (int_of_string s))
      |> Array.of_list
    in
    res.(i) <- line;
  done;
  (n, res)
;;

let print_sudoku grid =
  Array.iter (
    fun row ->
      Array.iter (
        fun element ->
          match element with
            | Domain l -> (print_string "{"; List.iter (Printf.printf "%d ") l; print_string "} ")
            | Fixed e -> Printf.printf "%d " e
      )
      row; print_newline ();
  )
  grid
;;

let make_new_domain e = function
  | Domain d -> (
      let new_d = List.filter ((<>) e) d in
      Domain new_d
    )
  | f -> f
;;

let delete_in dir p n e grid =
  for i = 0 to n - 1 do
    match dir with
      | Lin -> grid.(p).(i) <- (make_new_domain e grid.(p).(i))
      | Col -> grid.(i).(p) <- (make_new_domain e grid.(i).(p))
  done;
;;

let delete_square x y n e grid =
  let len = sqrt (float_of_int n) |> int_of_float in
  let nx, ny = x - (x mod len), y - (y mod len) in
  for i = ny to ny + len - 1 do
    for j = nx to nx + len - 1 do
      grid.(i).(j) <- make_new_domain e grid.(i).(j)
    done;
  done;
;;

let propagation n grille =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let a = grille.(i).(j) in
      match a with
        | Fixed e -> (
            delete_in Col j n e grille;
            delete_in Lin i n e grille;
            delete_square j i n e grille;
        )
        | _ -> ()
    done;
  done;
;;
  
let stop_condition grid =
  Array.exists (fun a -> 
    Array.exists (fun d -> 
      match d with 
        | Fixed _ -> false 
        | Domain _ -> true) 
      a
    ) grid
;;

let coord_of_minimal_domain n grid =
  let min, x, y, dom = ref n, ref 0, ref 0, ref [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let res, len, domain = match grid.(i).(j) with Fixed _ -> (false, 0, []) | Domain l -> (true, List.length l, l) in
      if res && (len < !min || !dom = [])
      then (
        min := len;
        x := j;
        y := i;
        dom := domain;
      )
    done;
  done;
  (!x, !y, !dom)
;;

let has_empty_domain grid =
  Array.exists (fun row ->
    Array.exists (function Domain [] -> true | _ -> false) row
  ) grid
;;

let copy_grid g =
  Array.map Array.copy g
;;

let rec backtrack ?(debug=false) n grid =
  propagation n grid;
  if debug then (print_sudoku grid; print_newline ());

  if has_empty_domain grid then false
  else if not (stop_condition grid) then (print_sudoku grid; true)
  else
    let x, y, dom = coord_of_minimal_domain n grid in
    let rec aux = function
      | [] -> false
      | hd::tl -> (
          let grid' = copy_grid grid in
          grid'.(y).(x) <- Fixed hd;
          let res = backtrack n grid' in
          if res
          then true
          else
            aux tl
      )
    in
    aux dom
;;

    
let solve n grid =
  let res_back = backtrack n grid in
  if res_back
  then (
    Printf.printf "solved !\n"
  )
  else
    Printf.printf "Not possible to solve."
;;

let solver path =
  let n, grid = read_instance path in
  if is_valid_grid n grid
  then
    solve n grid
  else
    Printf.printf "Grid is not valid.\n"
;;