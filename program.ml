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

  let reverse list =
    let rec reverse_aux acc list =
      match list with
        | [] -> acc
        | head :: tails -> reverse_aux (head :: acc) tails
    in
    reverse_aux [] list

  let lines_blanks str =
    let lines = String.split_on_char '\n' str in
    let rec aux acc acc' = function
      | [] -> List.rev ((String.sub acc' 1 (String.length acc' - 1)) :: acc)
      | x :: xs -> 
        if x = "" then aux ((String.sub acc' 1 (String.length acc' - 1)):: acc) "" xs
        else aux acc (acc' ^ "\n" ^ x) xs
    in
    aux [] "" lines
  
  (* pobrano iz https://stackoverflow.com/questions/2378947/how-do-i-intersect-two-lists-in-ocaml*)
  let intersect l1 l2 =
    List.fold_left (fun acc x -> if (List.exists (fun y -> y = x) l1) then x::acc else acc) [] l2;;
  
  (* pobrano iz https://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-in-ocaml *)
  let rec slice b e l = 
  match l with
    [] -> failwith "wrong boundaries"
  | h :: t -> 
     let tail = if e=0 then [] else slice (b-1) (e-1) t in
     if b>0 then tail else h :: tail

  (* pobrano iz https://stackoverflow.com/questions/37091784/ocaml-function-replace-a-element-in-a-list *)
  let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

end

module String = struct
  include String

  (* dobil iz https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317 *)
  let list_of_char string = string |> String.to_seq |> List.of_seq
  
  let string_of_char_list list =
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux (acc ^ Char.escaped x) xs
    in
    aux "" list
  
  let string_of_string_list list =
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> 
        if acc = "" then aux x xs
        else aux (acc ^ " " ^ x) xs
    in
    aux "" list

  let remove n str =
    let list = list_of_char str in
    let filter_fun = fun i _ -> i != n in
    let list = List.filteri filter_fun list in
    string_of_char_list list

end

(* pomožne funkcije *)
module Aux = struct

  let rec counter acc condition = function
    | x :: xs ->
      if condition x then counter (acc + 1) condition xs
      else counter acc condition xs
    | [] -> acc

  (* dobil iz https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#OCaml *)
  let is_int s = try ignore (int_of_string s); true with _ -> false

  let rec max f acc = function
    | x :: xs -> 
      if f x > snd acc then max f (x, f x) xs
      else max f acc xs
    | [] -> acc

  let rec range lower upper =
    if lower = upper then [] else
    lower :: (range (lower + 1) upper)

  (* dobil iz https://stackoverflow.com/questions/25980927/how-to-write-a-function-to-find-max-number-in-a-list-of-type-number-in-ocaml *)
  let max_in_list l = 
  match l with
  [] -> failwith "None"
  |h::t ->  let rec helper (seen,rest) =
              match rest with 
              [] -> seen
              |h'::t' -> let seen' = if h' > seen then h' else seen in 
                         let rest' = t'
              in helper (seen',rest')
            in helper (h,t)

  let min_in_list l = 
  match l with
  [] -> failwith "None"
  |h::t ->  let rec helper (seen,rest) =
              match rest with 
              [] -> seen
              |h'::t' -> let seen' = if h' < seen then h' else seen in 
                         let rest' = t'
              in helper (seen',rest')
            in helper (h,t)

end

module Print = struct

  let char_list list =
    print_string "[";
    let rec aux list =
    match list with
      | e :: [] -> print_char (e); print_string "]"
      | e::l -> print_char (e); print_string ";"; print_string " "; aux l
      | [] -> print_string "[]"
    in
    aux list
  
  let string_list list =
    print_string "[";
    let rec aux list =
    match list with
      | e :: [] -> print_string (e); print_string "]"
      | e::l -> print_string (e); print_string ";"; print_string " "; aux l
      | [] -> print_string "[]"
    in
    aux list

  let int_list list =
    print_string "[";
    let rec aux list =
    match list with
      | e :: [] -> print_int (e); print_string "]"
      | e::l -> print_int (e); print_string ";"; print_string " "; aux l
      | [] -> print_string "[]"
    in
    aux list

  let print_list_list list =
    print_string "[";
    let rec aux list =
    match list with
      | e :: [] -> string_list (e); print_string "]"
      | e::l -> string_list (e); print_string ";"; print_string " "; aux l
      | [] -> print_string "[]"
    in
    aux list;

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

module Solver4 : Solver = struct
  
  let list_of_lists line = 
    let list = List.tl (String.split_on_char ' ' line) in
    let rec aux acc = function
      | x :: xs -> aux ((String.split_on_char ':' x) :: acc) xs
      | [] -> acc
    in
    aux [] list
  
  module Passport = Map.Make(String);;

  let passport_of_line line =
    let list = list_of_lists line in
    let dict = Passport.empty in
    let rec fill_dict dict = function
      | x :: xs ->
        let dict' = Passport.add (List.hd x) (List.hd (List.tl x)) dict in
        fill_dict dict' xs
      | [] -> dict
    in
    fill_dict dict (list)

  let valid_passport dict =
    match Passport.cardinal dict with
      | 8 -> true
      | 7 -> if not (Passport.mem "cid" dict) then true else false
      | _ -> false
    
  let naloga1 data =
    let lines = data |> List.lines_blanks |> List.map passport_of_line in
    string_of_int (Aux.counter 0 valid_passport lines)

  let valid_byr byr =
    1920 <= int_of_string byr && int_of_string byr <= 2002

  let valid_iyr iyr =
    2010 <= int_of_string iyr && int_of_string iyr <= 2020

  let valid_eyr eyr =
    2020 <= int_of_string eyr && int_of_string eyr <= 2030

  let rec valid_hcl hcl =
    let characters = String.list_of_char hcl in
    if List.hd characters != '#' || List.length characters != 7  then false else
    let rec aux = function
      | [] -> true
      | x :: xs -> 
        match List.mem x ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'] with
          | false -> false
          | true -> aux xs 
    in
    aux (List.tl characters)

  let valid_hgt hgt =
    let inches = String.split_on_char 'i' hgt in
    let cm = String.split_on_char 'c' hgt in
    match List.length inches, List.length cm with
      | 2, 1 -> if 59 <= int_of_string (List.hd inches) && int_of_string (List.hd inches) <= 76 then true else false
      | 1, 2 -> if 150 <= int_of_string (List.hd cm) && int_of_string (List.hd cm) <= 193 then true else false
      | _ -> false

  let valid_ecl ecl =
    List.mem ecl ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

  let valid_pid pid =
    String.length pid = 9 && Aux.is_int pid

  let valid_passport' dict =
    let byr = Passport.find_opt "byr" dict in
      if Option.is_none byr then false else
      let byr = Option.get byr in
    let iyr = Passport.find_opt "iyr" dict in
      if Option.is_none iyr then false else
      let iyr = Option.get iyr in 
    let eyr = Passport.find_opt "eyr" dict in
      if Option.is_none eyr then false else
      let eyr = Option.get eyr in 
    let hgt = Passport.find_opt "hgt" dict in
      if Option.is_none hgt then false else
      let hgt = Option.get hgt in 
    let hcl = Passport.find_opt "hcl" dict in
      if Option.is_none hcl then false else
      let hcl = Option.get hcl in 
    let ecl = Passport.find_opt "ecl" dict in
      if Option.is_none ecl then false else
      let ecl = Option.get ecl in 
    let pid = Passport.find_opt "pid" dict in
      if Option.is_none pid then false else
      let pid = Option.get pid in 
    
    if not (valid_byr byr) then false else
    if not (valid_iyr iyr) then false else
    if not (valid_eyr eyr) then false else
    if not (valid_hgt hgt) then false else
    if not (valid_hcl hcl) then false else
    if not (valid_ecl ecl) then false else
    if not (valid_pid pid) then false else true
 
  let naloga2 data _part1 =
    let lines = data |> List.lines_blanks |> List.map passport_of_line in
    string_of_int (Aux.counter 0 valid_passport' lines)

end

module Solver5 : Solver = struct

  let from_binary string = 
    let characters = String.list_of_char string in
    let rec aux acc i = function
      | [] -> acc
      | x :: xs -> 
        match x with
          | ('F' | 'L') -> aux acc (2 * i) xs
          | ('B' | 'R')  -> aux (acc + i) (2 * i) xs
          | _ -> failwith "impossible"
    in
    aux 0 1 (List.reverse characters)
  
  let id string =
    let row = String.sub string 0 7 in
    let column = String.sub string 7 3 in
    from_binary row * 8 + from_binary column

  let naloga1 data =
    let lines = List.lines data in
    let n = snd (Aux.max id ("", 0) lines) in
    string_of_int n

  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.map id in
    let rec find = function
      | [] -> failwith "impossible"
      | x :: xs ->
        match List.mem (x - 1) xs, List.mem (x - 2) xs with
          | false, true -> (x - 1)
          | _ -> find (xs @ [x])
    in
    string_of_int (find lines)

end

module Solver6 : Solver = struct

  let count_in_group group =
    let characters = String.list_of_char group in
    let rec make_list acc = function
      | x :: xs -> 
        if List.mem x acc || x = '\n' || x = ' ' then make_list acc xs
        else make_list (x :: acc) xs
      | [] -> acc
    in
    List.length (make_list [] characters)

  let naloga1 data =
    let groups = List.lines_blanks data in
    let rec count acc = function
    | x :: xs -> count (acc + count_in_group x) xs
    | [] -> acc 
    in
    string_of_int (count 0 groups)

  let count_in_group' group =
    let lines = group |> List.lines |> List.map String.list_of_char in
    let filter_fun = fun y -> match y with | '\n' -> false | ' ' -> false | _ -> true in
    let rec aux acc = function
      | x :: xs -> 
        let x = List.filter filter_fun x in
        aux (List.intersect acc x) xs
      | [] -> List.length acc
    in
    aux (List.hd lines) lines

  let naloga2 data _part1=
    let groups = List.lines_blanks data in
    let rec count acc = function
    | x :: xs -> count (acc + count_in_group' x) xs
    | [] -> acc 
    in
    string_of_int (count 0 groups)

end

(* unfinished
module Solver7 : Solver = struct

  let tuple_of_bag string =
    let words = String.split_on_char ' ' string in 
    let words = (match List.hd words with 
      | "" -> List.slice 1 (List.length words - 2) words
      | "no" -> ["0"]
      | _ -> List.slice 0 (List.length words - 2) words) in
    let int = int_of_string (List.hd words) in
    let bag' = String.string_of_string_list (List.tl words) in
    (int, bag')

  let split line =
    let words = String.split_on_char ' ' line in
    let bag_list = List.slice 0 1 words in
    let bags_list = List.slice 4 (List.length words - 1) words in
    let bag = String.string_of_string_list bag_list in
    let bags = String.string_of_string_list bags_list in
    let bags_list = String.split_on_char ',' bags in
    (bag, List.map tuple_of_bag bags_list)
  
  let contains_bags bag = function
    | (y, list) -> List.map snd list

  let rec contains_bag bag tuple =
    (*| (y, list) -> 
      match list with
        | x :: xs -> if snd x = bag then true else contains_bag bag (y, xs)
        | [] -> false*)
    List.mem bag (contains_bags bag tuple)

  let rec bags_that_direct_contain bag acc = function
    | x :: xs -> 
      if contains_bag bag x then bags_that_contain bag ((fst x) :: acc) xs
      else bags_that_contain bag acc xs
    | [] -> acc

  let rec bags_that_contain bag lines =
    let direct = bags_that_direct_contain bag 0 lines in
    let rec stevec acc spomin = function
      | [] -> acc
      | b::bs ->
      if List.exists (fun x -> x = b) spomin then stevec acc spomin bs
      else stevec (acc+1) (b::spomin) (bs @ (vsebujejo b bags))
    in

  let naloga1 data =
    let lines = data |> List.lines |> List.map split in
    
    let rec list acc = function
      | x :: xs -> 
        if contains_bag "shiny gold" x then list ((fst x) :: acc) xs
        else list acc xs
      | [] -> acc
    in
    let list = "shiny gold" :: (list [] lines) in
    Print.string_list list;
    
    let rec aux acc = function
      | [] -> acc
      | x :: xs ->
        match x with
          (bag, l) ->
          let l = List.map snd l in
          if List.length (List.intersect l list) = 0 then aux acc xs
          else aux (acc + 1) xs
    in
    string_of_int (aux 0 lines)


  let naloga2 data _part1 =
    let lines = data |> List.lines in
    ""

end *)

module Solver8 : Solver = struct

  let line_to_tuple line =
    let list = String.split_on_char ' ' line in
    let fst = List.hd list in
    let snd = int_of_string (List.hd (List.tl list)) in
    (fst, snd)

  let enumerate lines =
    let list = List.lines lines in
    let rec aux acc i = function
      | x :: xs -> 
        let x = line_to_tuple x in
        let enum = (i, x) in
        aux (enum :: acc) (i + 1) xs
      | [] -> List.reverse acc
    in
    aux [] 0 list
  
  let rec move n acc acc' list = 
    let (i, tuple) = List.nth list n in
    if List.mem i acc' then (acc, true) else
    if List.length list <= n + 1 then (acc, false) else
    match tuple with
      | ("nop", _) -> move (n + 1) acc (n :: acc') list
      | ("acc", j) -> move (n + 1) (acc + j) (n :: acc') list
      | ("jmp", j) -> move (n + j) acc (n :: acc') list
      | _ -> failwith "impossible"

  let naloga1 data =
    let lines = data |> enumerate in
    let sol = move 0 0 [] lines in
    string_of_int (fst sol)

  let change_tuple tuple =
    let (n, (x, i)) = tuple in
    match x with
      | "nop" -> (n, ("jmp", i))
      | "jmp" -> (n, ("nop", i))
      | "acc" -> tuple
      | _ -> failwith "ˇimpossible"
    
  let naloga2 data _part1 =
    let lines = data |> enumerate in
    let rec aux lines i =
      let x = List.nth lines i in
      let x' = change_tuple x in
      let lines' = List.replace lines i x' in
      let acc, bool = move 0 0 [] lines' in
      if bool then aux lines (i + 1)
      else acc
    in
    string_of_int (aux lines 0)

end 

module Solver9 : Solver = struct
  
  let is_valid n list =
    let rec aux n i = function
      | [] -> false
      | x :: xs -> 
        match i, List.mem (n - x) xs with
          | 0, _ -> false
          | _, true -> true
          | _, _ -> aux n (i - 1) (xs @ [x])
    in
    aux n (List.length list) list

  let naloga1 data =
    let lines = data |> List.lines |> List.int_list in
    let len = 25 in
    let start_list = List.slice 0 (len - 1) lines in
    let start_tail = List.slice len ((List.length lines) - 1) lines in
    let rec aux list tail =
      let len = List.length list in
      match tail with
        | x :: xs -> 
          if not (is_valid x list) then x else
          let list' = (List.slice 1 (len - 1) list) @ [x] in
          aux list' xs 
        | [] -> failwith "impossible"
    in
    string_of_int (aux start_list start_tail)

  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.int_list in
    let n = int_of_string (naloga1 data) in
    let rec aux n list lines =
      match lines with
        | [] -> failwith "impossible"
        | x :: xs -> 
          match List.sum (x :: list) with
            | i when i > n -> aux n [] ((List.tl (List.reverse list)) @ lines)
            | i when i = n -> (x :: list)
            | _ -> aux n (x :: list) (List.tl lines)
    in
    let list = aux n [] lines in
    let min = Aux.min_in_list list in
    let max = Aux.max_in_list list in
    string_of_int (min + max)
    
end

module Solver10 : Solver = struct

  let tuple_1_2_3 lines =
    let lines = lines |> List.sort Stdlib.compare |> List.reverse in
    let rec aux acc1 acc2 acc3 = function
      | [] -> (acc1, acc2, acc2)
      | x :: [] -> if x = 1 then (acc1 + 1, acc2, acc3) else (acc1, acc2, acc2)
      | x1 :: x2 :: xs -> 
        match x1 - x2 with
          | 1 -> aux (1 + acc1) acc2 acc3 (x2 :: xs)
          | 2 -> aux acc1 (1 + acc2) acc3 (x2 :: xs)
          | 3 -> aux acc1 acc2 (1 + acc3) (x2 :: xs)
          | _ -> failwith "impossible"   
    in
    aux 0 0 0 lines

  let naloga1 data =
    let lines = data |> List.lines |> List.int_list in
    let n1, n2, n3 = tuple_1_2_3 lines in
    print_int n1; print_string " "; print_int n3;
    string_of_int ((n3 + 1) * n1)

  let naloga2 data _part1 =
    ""

end  

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "6" -> (module Solver6)
  | "8" -> (module Solver8)
  | "9" -> (module Solver9)
  | "10" -> (module Solver10)
  | _ -> failwith "Not solved yet"

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
