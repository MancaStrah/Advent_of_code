let dan = "7"

let data_to_list vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke

let rec check_others list others =
  match others with
  | [] -> list
  | n :: a :: c :: b :: rest -> check_others ((int_of_string n, a ^ c) :: list) rest
  | _ -> failwith "Ojoj, check_others"


let make_line_nice line =
  match String.split_on_char ' ' line with
  | a :: c :: bags :: contain :: n :: others -> (
    let main_bag = a ^ c in 
    let contains_others = match n with | "no" -> false | _ -> true in
    if contains_others then (
    let other_bags = check_others [] (n :: others) in
    (0, main_bag) :: other_bags
    )
    else
    [(0, main_bag)]
  )
  | _ -> failwith "Ojoj, make_bag_nice"

let make_nice_lines_list lines =
  List.map make_line_nice lines
 
let rec shiny_gold_in_line line_list = 
  match line_list with
  | [] -> false
  | (n, c) :: xs -> if c = "shinygold" then true else shiny_gold_in_line xs

let rec check_tuple_list colour tuple_list =
  (* Checks if tuple list contains tuple (_, colour) *)
  match tuple_list with 
  | [] -> false
  | (n, col) :: others -> if col = colour then true else check_tuple_list colour others

let rec has_one_that_contains_shiny_gold_in_line shiny_gold_list line = 
  match shiny_gold_list with
  | [] -> false
  | c :: cols -> (
    if check_tuple_list c line then true else 
    has_one_that_contains_shiny_gold_in_line cols line
  )

let rec shiny_gold_list lines =
  let rec aux list lines = 
    match lines with 
    | [] -> list
    | x :: xs -> (
      if shiny_gold_in_line x then (
        match x with
        | (n, c) :: others -> aux (c :: list) xs 
        | _ -> failwith "Ojoj, shiny_gold_list"
      )
      else aux list xs)
  in
  aux [] lines

(* neka ne superiorna koda *)
(* | [] -> [] *)
(* | x :: xs -> *)
    (* if shiny_gold_in_line x then *)
    (* let (0, colour) :: _ = x in *)
    (* colour :: shiny_gold_list xs *)
    (* else shiny_gold_list xs *)


let not_shiny shiny_list lines = 
  let rec aux list shiny_list lines  = 
    match lines with 
    | [] -> list
    | x :: xs -> (
      match x with
      | (n, col) :: rest -> if List.mem col shiny_list then aux list shiny_list xs
      else aux (x :: list) shiny_list xs
      | _ -> failwith "ojoj, not_shiny"
    )
  in
  aux [] shiny_list lines


let rec check_not_shiny_line shiny_list not_shiny_line =
  match not_shiny_line with
  | [] -> false
  | x :: xs -> (
    match x with
    | (n, col) -> if List.mem col shiny_list then true else check_not_shiny_line shiny_list xs
  )

let check_shiny shiny_colours not_shiny_lines =
  let rec update shiny_colours not_shiny_lines new_not_shiny_lines = 
    match not_shiny_lines with
    | [] -> (shiny_colours, new_not_shiny_lines)
    | x :: xs -> (
      if check_not_shiny_line shiny_colours x then (
        match x with
        | (n, col) :: others -> (
          if List.mem col shiny_colours then update shiny_colours xs new_not_shiny_lines
          else update (col :: shiny_colours) xs new_not_shiny_lines
        )
        | _ -> failwith "ojoj, check_shiny"
      )
      else update shiny_colours xs (x :: new_not_shiny_lines)
      )
  in 
  update shiny_colours not_shiny_lines []
    
 let update_until_the_end lines = 
  let shiny_colours = shiny_gold_list lines in 
  let not_shiny_lines = not_shiny shiny_colours lines in 
  let rec aux shiny not_shiny =
    let (updated_s, updated_n) = check_shiny shiny not_shiny in
    let l =  List.length shiny in
    let lu = List.length updated_s in 
    if lu = l then l else aux updated_s updated_n
  in
  (aux shiny_colours not_shiny_lines) - 1
  (* Ta -1 je zato, ker shiny gold torbo tudi štejemo, ona pa ne more vsebovati še ene shiny gold torbe, 
  torej svoje shiny gold torbe ne moremo dati vanjo *)


let naloga1 vsebina_datoteke =
  let lines = data_to_list vsebina_datoteke in
  let nice_lines = make_nice_lines_list lines in 
  string_of_int (update_until_the_end nice_lines)



(* Pomožne funkcije za 2. nalogo *)

let make_line_nice2 line =
  match String.split_on_char ' ' line with
  | a :: c :: bags :: contain :: n :: others -> (
    let main_bag = a ^ c in
    let contains_others = match n with | "no" -> false | _ -> true in
    if contains_others then (
    let other_bags = check_others [] (n :: others) in
    (main_bag, other_bags)
    )
    else
    (main_bag, [])
  )
  | _ -> failwith "Ojoj, make_bag_nice"

let make_nice_lines_list2 lines =
  List.map make_line_nice2 lines

let rec get_line colour lines =
  match lines with
  | [] -> failwith "ojoj"
  | (col, oth) :: xs -> if col = colour then (col, oth) else get_line colour xs

let rec check_colour (colour, rest) lines =
  match rest with
  | [] -> 1 
  | (n, c) :: xs -> (n * (check_colour (get_line c lines) lines)) + check_colour (colour, xs) lines


let naloga2 vsebina_datoteke =
  let lines = data_to_list vsebina_datoteke in 
  let nice_lines2 = make_nice_lines_list2 lines in
  let shiny = make_line_nice2 "shiny gold bags contain 5 mirrored crimson bags, 5 mirrored tan bags, 5 drab green bags, 5 shiny silver bags." in
  string_of_int ((check_colour shiny nice_lines2) - 1)
  (* Ker ona ne vsebuje sama sebe, je ta -1 *)


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