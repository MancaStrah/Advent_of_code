let dan = "1"


let naloga1 vsebina_datoteke =
  let lines = String.split_on_char '\n' vsebina_datoteke in
  let list = List.map int_of_string lines in
	let rec aux list =
  match list with
		| x :: xs ->
			if List.mem (2020 - x) xs then string_of_int (x * (2020 - x)) else aux xs 
		| _ -> "Ojoj" in
	aux list


let naloga2 vsebina_datoteke =
	let lines = String.split_on_char '\n' vsebina_datoteke in
  let list = List.map int_of_string lines in
	let rec check_tail x tail = 
		match tail with
			| b :: bs ->
			if List.mem (2020 - x - b) bs then Some((b * (2020 - x - b))) else check_tail x bs 
			| _ -> None in
	let rec aux list =
		match list with
		| x :: xs -> 
			(match check_tail x xs with
				| Some m -> string_of_int (m * x)
				| None -> aux xs)
		| _ -> "Ojoj" in 
	aux list

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