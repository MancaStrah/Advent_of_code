#load "str.cma"

let dan = "4"

let data_to_list vsebina_datoteke = 
  let lines = String.split_on_char '\n' vsebina_datoteke in
  lines

let split_by_colon_and_take_first string =
  let new_string = String.split_on_char ':' string in
  match new_string with
  | x :: xs -> x
  | _ -> failwith "Ojoj"

let join_lines lines = 
  let rec aux new_list person lines = 
    match lines with
    | [] -> (person :: new_list)
		| x :: xs -> (
			match x with
      | "" -> aux (person :: new_list) [] xs
      | line ->
      let line_list = String.split_on_char ' ' line in 
      aux new_list (line_list @ person) xs)
  in
  aux [] [] lines

let check_person person_list = 
  let passport_list = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] in
  let rec aux person_list passport_list = 
  match passport_list with
  | [] -> true
  | x :: xs ->
    if List.mem x person_list then aux person_list xs 
    else false
  in
  aux person_list passport_list


let naloga1 vsebina_datoteke =
  let lines_list = data_to_list vsebina_datoteke in
  let person_lines = join_lines lines_list in
  let rec aux count person_lines = 
    match person_lines with
    | [] -> count
    | x :: xs ->
      let person = List.map split_by_colon_and_take_first x in
      if check_person person then aux (count + 1) xs
      else aux count xs
  in
string_of_int (aux 0 person_lines)

(* Pomožne funkcije za drugo nalogo. *)


let check_byr y =
let year = int_of_string y in 
((1920 <= year) && (year <= 2002))

let check_iyr y =
let year = int_of_string y in 
((2010 <= year) && (year <= 2020))

let check_eyr y =
let year = int_of_string y in 
((2020 <= year) && (year <= 2030))

let check_hgt height =
  let r = Str.regexp "cm" in 
  try ignore (Str.search_forward r height 0); (
    let number = String.sub height 0 ((String.length height) - 2) in 
    try ((150 <=(int_of_string number)) && ((int_of_string number) <= 193))
    with Failure _ -> false
  )
  with Not_found -> (
    let r' = Str.regexp "in" in 
    try ignore (Str.search_forward r' height 0); (
      let number = String.sub height 0 ((String.length height) - 2) in 
      try ((59 <=(int_of_string number)) && ((int_of_string number) <= 76))
      with Failure _ -> false
    )
    with Not_found -> false
    )


let check_hcl colour =
  let r = Str.regexp "#[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]$" in 
  try ignore (Str.search_forward r colour 0); true 
  with Not_found -> false

let check_ecl colour = 
  match colour with
  | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
  | _ -> false

let check_pid number = 
  let r = Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" in 
  try ignore (Str.search_forward r number 0); true 
  with Not_found -> false

let match_string string = 
  match String.split_on_char ':' string with
  | [name; value] -> (
    match name with
    | "byr" -> check_byr value
    | "iyr" -> check_iyr value
    | "eyr" -> check_eyr value
    | "hgt" -> check_hgt value
    | "hcl" -> check_hcl value
    | "ecl" -> check_ecl value
    | "pid" -> check_pid value
    | "cid" -> true
    | _ -> false)
  | _ -> failwith "Ojoj"

let check_valid person_list = 
  let passport_list = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] in
  let rec aux person_list passport_list = 
  match passport_list with
  | [] -> true
  | x :: xs ->
    if (List.mem x (List.map split_by_colon_and_take_first person_list)) then aux person_list xs 
    else false
  in
  aux person_list passport_list
  

let rec is_ok person_list =
    match person_list with
    | [] -> true
    | x :: xs -> 
    if match_string x then is_ok xs else false


let naloga2 vsebina_datoteke = 
let lines_list = data_to_list vsebina_datoteke in
  let person_lines = join_lines lines_list in
  let rec aux count person_lines =
    match person_lines with
    | [] -> count
    | x :: xs ->
      if check_valid x then 
        (if is_ok x then aux (count + 1) xs else aux count xs)
      else aux count xs
  in
string_of_int (aux 0 person_lines)



let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko ("data/day_" ^ dan ^ ".in") in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko ("out/day_" ^ dan ^ "_1.out") odgovor1;
    izpisi_datoteko ("out/day_" ^ dan ^ "_2.out") odgovor2