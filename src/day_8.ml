let dan = "8"

let data_to_list vsebina_datoteke = 
  let lines = String.split_on_char '\n' vsebina_datoteke in
  lines

let make_nice_list list =
  let rec make_list i new_list list =
  match list with
  | [] -> new_list
  | x :: xs -> (
    let line_list = String.split_on_char ' ' x in 
    let inst = (
      match line_list with
      | ins :: rest -> ins
      | _ -> failwith "ojoj, make_line_nice"
    )
    in
    let number = (
      match line_list with
      | ins :: rest :: [] -> int_of_string rest
      | _ -> failwith "Ojoj, make_line_nice"
    )
    in 
  make_list (i + 1) ((i, (inst, number)) :: new_list ) xs
  )
  in
  make_list 0 [] list


let rec aux count used_list index lines = 
  let start = (index, List.assoc index lines) in
  let (i, (ins, num)) = start in
  if List.mem i used_list then count
  else (
    match ins with
    | "nop" -> aux count (i :: used_list) (i + 1) lines
    | "acc" -> aux (count + num) (i :: used_list) (i + 1) lines
    | "jmp" -> aux count (i :: used_list) (i + num) lines 
    | _ -> failwith "Ojoj aux"
    )

let naloga1 vsebina_datoteke =
  string_of_int (aux 0 [] 0 (make_nice_list (data_to_list vsebina_datoteke)))



(* Pomožne funkcije za 2. nalogo. *)

let rec change i list =
  match list with
  | [] -> []
  | (k, ("jmp", x)) :: xs ->
  if k = i then (k, ("nop", x)) :: xs else (k, ("jmp", x)) :: change i xs
  | (k, ("nop", x)) :: xs ->
  if k = i then (k, ("jmp", x)) :: xs else (k, ("nop", x)) :: change i xs
  | (k, ("acc", x)) :: xs -> (k, ("acc", x)) :: change i xs
  | _ -> failwith "Ojoj, change"

(* Podobno kot v prvi nalogi *)
let rec test count used_list index lines = 
  let start = (index, List.assoc index lines) in
  let (i, (ins, num)) = start in
  if index = 640 then count else (
  if List.mem i used_list then 0
  else (
    match ins with
    | "nop" -> test count (i :: used_list) (i + 1) lines
    | "acc" -> test (count + num) (i :: used_list) (i + 1) lines
    | "jmp" -> test count (i :: used_list) (i + num) lines 
    | _ -> failwith "Ojoj aux"
    ))

let check lines =
  let rec aux i lines =
    let changed_list = change i lines in 
    let count = test 0 [] 0 changed_list in 
    (if (count = 0) then (aux (i + 1) lines) else count)
  in
  aux 0 lines

let naloga2 vsebina_datoteke = 
  let lines = data_to_list vsebina_datoteke in 
  let nice_lines = make_nice_list lines in 
  string_of_int (check nice_lines)
  
  
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
















(* let change_line line = 
  let (i, (ins, num)) = line in 
  match ins with
  | "nop" -> (i, ("jmp", num))
  | "jmp" -> (i, ("nop", num))
  | _ -> line
 
let rec make_new_list index new_list lines = 
  match lines with 
  | [] -> List.rev new_list
  | x :: xs -> 
    match x with 
    | (i, (ins, num)) -> (
      if i = index then make_new_list index ((change_line x)  :: new_list) xs
      else make_new_list index  new_list xs
    )
    | _ -> failwith "Ojoj, make_new_list" *)

