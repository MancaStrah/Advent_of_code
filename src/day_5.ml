let dan = "5"

let strip_first_char str =
  if str = "" then "" else
  String.sub str 1 ((String.length str) - 1)

let data_to_list vsebina_datoteke = 
  let lines = String.split_on_char '\n' vsebina_datoteke in
  lines

let split (low, high) char =
	let half = ((high + low) / 2 ) in 
	match char with
	| "F" | "L" -> (
		let new_high = half in
		(low, new_high)
	)
	| "B" | "R" -> (
		let new_low = (half + 1) in
		(new_low, high)
	)
	| _ -> failwith "Ojoj"

let check_row line_string =
let row_letters = String.sub line_string 0 7 in
	let rec aux line_string (low, high) =
		match line_string with
		| "" -> low
		| string -> (
			match string.[0] with
			| 'B' -> (
				let (l, h) = split (low, high) "B" in
				if l = h then l else 
				aux (strip_first_char string) (l, h)
				)

			| 'F' -> (
				let (l, h) = split (low, high) "F" in
				if l = h then l else 
				aux (strip_first_char string) (l, h)
			)
			| _ -> failwith "Ojoj"
		)
	in
	aux row_letters (0, 127)


let check_seats line_string =
let seat_letters = String.sub line_string 7 3 in
	let rec aux line_string (low, high) = 
		match line_string with
		| "" -> low
		| string -> (
			match string.[0] with
			| 'R' -> (
				let (l, h) = split (low, high) "R" in
				if l = h then l else 
				aux (strip_first_char string) (l, h)
				)

			| 'L' -> (
				let (l, h) = split (low, high) "L" in
				if l = h then l else 
				aux (strip_first_char string) (l, h)
			)
			| _ -> failwith "Ojoj"
		)
	in
	aux seat_letters (0, 7)

let calculate_id row seat =
	(row * 8) + seat

let naloga1 vsebina_datoteke =
	let lines = data_to_list vsebina_datoteke in
	let rec aux id_list hihgest_id lines =
		match lines with
		| [] -> hihgest_id
		| x :: xs -> (
			let row = check_row x in
			let seat = check_seats x in 
			let id = calculate_id row seat in
	
			if hihgest_id <= id then aux (id :: id_list) id xs else aux  (id :: id_list) hihgest_id xs
		)
		in
	string_of_int(aux [] 0 lines)

(* Pomožne funkcije za 2. nalogo. *)
let get_id_list vsebina_datoteke =
	let lines = data_to_list vsebina_datoteke in
	let rec aux id_list lines =
		match lines with
		| [] -> id_list
		| x :: xs -> (
			let row = check_row x in
			let seat = check_seats x in
			let id = calculate_id row seat in
			aux (id :: id_list) xs)
		in
	aux [] lines

let xor a b = 
(a || b) && not (a && b)

let check_ids vsebina_datoteke=
	let id_list = get_id_list vsebina_datoteke in 
	let rec aux id_list id_list_to_cut =
		match id_list_to_cut with
		| [] -> failwith "Ojoj"
		| x :: xs -> (
			let one_less = List.mem (x - 1) id_list in
			let one_more = List.mem (x + 1) id_list in
			let two_less = List.mem (x - 2) id_list in
			let two_more = List.mem (x + 2) id_list in
			let a = (not one_more) && two_more in
			let b = (not one_less) && two_less in 
			let a_or_b = xor a b in
			if a_or_b then (
				if a then x + 1 else x - 1 
			)
			else aux id_list xs 
		)
	in
	string_of_int (aux id_list id_list )

let naloga2 vsebina_datoteke =
	check_ids vsebina_datoteke

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