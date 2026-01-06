open Sudoku_solver

let read_instance path =
  let take_line ic =
    input_line ic 
    |> String.split_on_char ' ' 
    |> List.filter_map (fun e -> if e = "" then None else Some (int_of_string e)) 
  in
  let ic = open_in path in
  let n = input_line ic |> int_of_string in
  let res : domain array array = Array.make n [|Fixed (-1)|] in
  let domain = take_line ic in
  let res_col = take_line ic |> Array.of_list in
  let res_lin = take_line ic |> Array.of_list in
  for i = 0 to n-1 do
    let line = 
      input_line ic 
      |> String.split_on_char ' ' 
      |> List.map (fun c -> 
          match c with 
            | "." -> Domain domain
            | s -> Fixed (int_of_string s))
      |> Array.of_list
    in
    res.(i) <- line;
  done;
  (n, res, res_col, res_lin)
;;

let print_fubuki n grid res_col res_lin =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let curr = grid.(i).(j) in
      match curr with
        | Domain l -> (
          if j = n - 1 then (print_string " {"; List.iter (Printf.printf "%d ") l; print_string "} ")
          else (print_string " {"; List.iter (Printf.printf "%d ") l; print_string "} +")
        )
        | Fixed e -> (
            if j = n - 1 then Printf.printf " %d " e
            else Printf.printf "%d +" e
        )
    done;
    Printf.printf "= %d\n" res_col.(i);
  done;

  print_newline ();

  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let curr = grid.(j).(i) in
      match curr with
        | Domain l -> if j = n - 1 then (print_string " {"; List.iter (Printf.printf "%d ") l; print_string "} ")
          else (print_string " {"; List.iter (Printf.printf "%d ") l; print_string "} +")
        | Fixed e -> (
            if j = n - 1 then Printf.printf " %d " e
            else Printf.printf "%d +" e
        )
    done;
    Printf.printf "= %d\n" res_lin.(i);
  done;
;;
      

let is_valid_grid n grid res_col res_lin =
  let col_cpy = Array.copy res_col in
  let lin_cpy = Array.copy res_lin in
  let tf = ref true in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      match grid.(i).(j) with
        | Fixed e when !tf = true -> (
            col_cpy.(j) <- col_cpy.(j) - e;
            lin_cpy.(i) <- lin_cpy.(i) - e;
        )
        | Domain _ -> tf := false
        | Fixed _ -> ()
    done;
  done;
  !tf && Array.for_all2 (fun a b -> a = 0 && b = 0) col_cpy lin_cpy
;;


let make_new_domain e = function
  | Domain d -> (
      let new_d = List.filter ((<>) e) d in
      Domain new_d
    )
  | f -> f
;;

let delete_all n element grid =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      grid.(i).(j) <- make_new_domain element grid.(i).(j);
    done;
  done;
;;

let propagation_fubuki n grid =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      match grid.(i).(j) with
        | Fixed e -> (
            delete_all n e grid;            
        )
        | _ -> ()
    done;
  done;
;;

let fn_sum_col n grid x hd =
  let sum = ref 0 in
  for i = 0 to n - 1 do
    let to_add =
      match grid.(i).(x) with
        | Fixed e -> e
        | _ -> hd
    in
    sum := to_add + !sum
  done;
  !sum
;;

let solve ?(debug=false) path =
  let n, grid, res_col, res_lin = read_instance path in
  let rec backtrack n grid =
    (* print_fubuki n grid res_col res_lin;
    print_newline (); *)
    propagation_fubuki n grid;
    
    if has_empty_domain grid then false
    else
      if not (stop_condition grid) then (print_fubuki n grid res_col res_lin; true)
      else
        let x, y, dom = coord_of_minimal_domain n grid in
        if debug then Printf.printf "(%d,%d)\n" x y;
        let rec aux = function
          | [] -> false
          | hd::tl -> (
              let grid' = copy_grid grid in
              let sum_col = 
                if x = n - 1 then
                  Some (Array.fold_left (fun r e -> (match e with Fixed e -> e  | _ -> hd ) + r) 0 grid'.(y)) 
                else
                  None
              in
              let sum_lin : int option =
                if y = n - 1 then
                  Some (fn_sum_col n grid' x hd)
                else
                  None
              in
              grid'.(y).(x) <- Fixed hd;
              
              if debug && (x = n - 1 || y = n - 1) then (
                print_fubuki n grid' res_col res_lin;
                print_newline ()
              );

              let sum_col_res = (match sum_col with
                | None -> true
                | Some s -> s = res_col.(y) 
              ) in

              let sum_lin_res = (match sum_lin with
                | None -> true
                | Some s -> s = res_lin.(x)
              ) in

              if not sum_col_res || not sum_lin_res
              then aux tl
              else
                let res = backtrack n grid' in
                if res
                then true
                else
                  aux tl
          )
        in
        aux dom
  in
  if backtrack n grid
  then print_endline "solved !"
  else print_endline "Impossible to solve."
;;