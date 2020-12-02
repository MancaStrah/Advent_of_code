let dan = "2"

let data_to_list vsebina_datoteke = 
  let lines = String.split_on_char '\n' vsebina_datoteke in
  lines

let min_and_max string =
  let list = String.split_on_char '-' string in
  match list with
  | a :: b :: [] -> (int_of_string a, int_of_string b)
  | _ -> (0, 0)

let count char string =
  let max = String.length string in
  let rec aux count char string i max =
    if i = max then count
    else
      if string.[i] = char then aux (count + 1) char string (i+1) max 
      else aux count char string (i+1) max
  in
  aux 0 char string 0 max

let check string =
    let list = String.split_on_char ' ' string in
    match list with
    | n :: l :: p :: [] -> (
      let (min, max) = min_and_max n in 
      let char = l.[0] in
      let password = p in 
      let number = count char password in
      if number <= max then
        if number >= min then true else false
      else false )
    | _ -> false


let naloga1 vsebina_datoteke =
  let list = data_to_list vsebina_datoteke in
  let rec aux count list = 
    match list with
    | x :: xs -> if check x then aux (count + 1) xs else aux count xs
    | [] -> count
  in 
  string_of_int (aux 0 list)

(* Pomožne funkcije za drugo nalogo. *)
let check_password2 a b char password = 
  let min = (a - 1) in 
  let max = (b - 1) in
  if password.[min] = char then
    (if password.[max] = char then false else true)
  else
    (if password.[max] = char then true else false)
  
let check2 string = 
let list = String.split_on_char ' ' string in
    match list with
    | n :: l :: p :: [] -> (
      let (min, max) = min_and_max n in 
      let char = l.[0] in
      let password = p in 
      let checked = check_password2 min max char password in 
      checked)
    | _ -> false 


let naloga2 vsebina_datoteke =
  let list = data_to_list vsebina_datoteke in
  let rec aux count list = 
    match list with
    | x :: xs -> if check2 x then aux (count + 1) xs else aux count xs
    | [] -> count
  in 
  string_of_int (aux 0 list)


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