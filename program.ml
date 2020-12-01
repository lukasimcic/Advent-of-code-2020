(* ker mi je že priložena funckija preberi_datoteko zmeraj sprožila napako End_of_file, sem spisal novo.
Pomožno funckijo read_lines sem dobil iz https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
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
          | (y, z) -> 
            print_string ((string_of_int x) ^ ("\n") ^ (string_of_int y) ^ ("\n") ^ (string_of_int z) ^ ("\n")); 
            x * y * z
    in
    string_of_int (aux lines)

end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
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
