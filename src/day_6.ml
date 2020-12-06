let dan = "6"

let data_to_list vsebina_datoteke = 
  let lines = String.split_on_char '\n' vsebina_datoteke in
  lines

let lines_to_group_string lines =
let rec aux group_strings group lines = 
    match lines with
    | [] -> group :: group_strings
    | x :: xs -> (
      match x with
      | "" -> aux (group :: group_strings) "" xs
      | string -> aux group_strings (string ^ group) xs
    )
  in
  aux [] "" lines

let count_different_chars_in_string string =
let alphabeth = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in
  let rec aux count alphabet string =
    match alphabet with
    | [] -> count
    | x :: xs -> (
      let is_in_string = String.contains string x in
      if is_in_string then aux (count + 1) xs string else
      aux count xs string 
    )
  in
  aux 0 alphabeth string 

let sum_of_all group_strings =
  let rec aux sum group_strings = 
    match group_strings with
    | [] -> sum
    | x :: xs -> aux (sum + count_different_chars_in_string x) xs 
  in
  aux 0 group_strings


let naloga1 vsebina_datoteke =
  let lines = data_to_list vsebina_datoteke in
  let group_strings = lines_to_group_string lines in 
  string_of_int (sum_of_all group_strings)

(* Pomožne funkcije za 2. nalogo *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
(* Vir: https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)

let lines_to_groups_list lines = 
  let rec aux group_list group lines = 
    match lines with
    | [] -> group :: group_list
    | x :: xs -> (
      match x with
      | "" -> aux (group :: group_list) [] xs
      | string -> aux group_list (explode(string) :: group) xs
    )
  in
  aux [] [] lines

let other_contain e others = 
  let rec aux e others = 
    match others with
    | [] -> true
    | x :: xs -> (
      if List.mem e x then aux e xs else false
    )
  in
  aux e others

let check_others e others = 
  let rec aux count e others =
    match e with
    | [] -> count
    | x :: xs -> (
      if other_contain x others then aux (count + 1 ) xs others
      else aux count xs others 
    )
  in
  aux 0 e others 

let look_group group = 
    match group with
    | [] -> 0
    | x :: xs -> check_others x xs

let check_groups group_list = 
  let rec aux count group_list =
    match group_list with
    | [] -> count
    | x :: xs -> aux (count + look_group x) xs
  in
  aux 0 group_list


let naloga2 vsebina_datoteke =
  let lines = data_to_list vsebina_datoteke in
  let group_list =  lines_to_groups_list lines in 
  string_of_int (check_groups group_list)


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