(* ker mi je že priložena funckija preberi_datoteko zmeraj sprožila napako End_of_file, sem spisal novo.
Pomožno funckijo read_lines sem dobil iz https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
let preberi_datoteko name = 
  let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
      try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s -> loop (s :: acc)
      | None -> close_in ic; List.rev acc in
    loop []
  in
  let rec str_list_to_str acc list = 
    match list with
      | s :: [] -> acc ^ s
      | s :: ss -> str_list_to_str (acc ^ s ^ "\n") ss
      | _ -> failwith "Prazna datoteka"
  in
  str_list_to_str "" (read_lines name)

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver1 : Solver = struct

  let rec sum_of_two_in_list_is_n n = function
    | x :: xs -> 
      let list = List.map (fun y -> x + y) xs in
      if List.mem n list then (x, n - x)
      else sum_of_two_in_list_is_n n xs
    | [] -> (-1, -1)

  let naloga1 data =
    let lines = data |> List.lines |> List.int_list in
    let (x, y) = sum_of_two_in_list_is_n 2020 lines in
    string_of_int (x * y)

  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.int_list in
    let rec aux = function
      | [] -> failwith "nemogoč primer"
      | x :: xs -> 
        match sum_of_two_in_list_is_n (2020 - x) xs with
          | (-1, -1) -> aux xs
          | (y, z) -> x * y * z
    in
    string_of_int (aux lines)

end  

module Solver2 : Solver = struct

  let line_to_tuple line = (* where line is like "3-17 s: ssjsssdssvwsssssss" *)
    match String.split_on_char ' ' line with
      | [n; char; password] -> (n, String.get char 0, password)
      | _ -> failwith "nemogoč primer"

  let limits n =  (* where n is like "3-17" *)
    let list n =  String.split_on_char '-' n in
    let lower_limit = List.hd (list n) in
    let upper_limit = List.hd (List.tl (list n)) in
    int_of_string lower_limit, int_of_string upper_limit

  let string_to_list_of_char str =
    List.init (String.length str) (fun i -> String.get str i)

  let password_is_valid line =
    let (n, char, password) = line_to_tuple line in
    let lower_limit, upper_limit = limits n in
    let rec count_el_in_list (el : char) acc = function
      | x :: xs -> 
        if x = el then count_el_in_list el (acc + 1) xs
        else count_el_in_list el acc xs
      | [] -> acc
    in
    let m = count_el_in_list char 0 (string_to_list_of_char password) in
    (lower_limit <= m) && (m <= upper_limit)
  
  let number_of_valid_passwords is_valid list =
    let rec aux acc = function
      | x :: xs -> 
        if is_valid x then aux (acc + 1) xs
        else aux acc xs
      | [] -> acc
    in
    aux 0 list

  let naloga1 data =
    let lines = List.lines data in
    let n = number_of_valid_passwords password_is_valid lines in
    string_of_int n

  let password_is_valid' line = 
    let (n, char, password) = line_to_tuple line in
    let lower_limit, upper_limit = limits n in
    let on_bottom_index = String.get password (lower_limit - 1) = char in
    let on_upper_index = String.get password (upper_limit - 1) = char in
    on_bottom_index != on_upper_index

  let naloga2 data _part1 =
    let lines = List.lines data in
    let n = number_of_valid_passwords password_is_valid' lines in
    string_of_int n

end

module Solver3 : Solver = struct

  let is_tree i line = String.get line i = '#'

  let rec counter acc len move_right move_down count_right count_down = function
    | [] -> acc
    | x :: xs ->
      match count_down mod move_down = 0, is_tree (count_right mod len) x with
        | false, _ -> counter acc len move_right move_down count_right (count_down + 1) xs
        | _, true -> counter (acc + 1) len move_right move_down (count_right + move_right) (count_down + 1) xs
        | _, false -> counter acc len move_right move_down (count_right + move_right) (count_down + 1) xs

  let naloga1 data =
    let lines = List.lines data in
    let length = String.length (List.hd lines) in
    let n = counter 0 length 3 1 0 0 lines in
    string_of_int n

  let naloga2 data _part1 =
    let lines = List.lines data in
    let length = String.length (List.hd lines) in
    let count right down = counter 0 length right down 0 0 lines in
    let n = (count 1 1) * (count 3 1) * (count 5 1) * (count 7 1) * (count 1 2) in
    string_of_int n

end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | _ -> failwith "Ni še rešeno"

let main () =
  (* Ker mi popravljen task s številom dneva ni delal, sem kodo popravil, tako da se dan vpiše ročno: *)
  print_string ("Choose the day, please: ");
  let day = read_line () in
  (* Konec mojega popravka. *)
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()
