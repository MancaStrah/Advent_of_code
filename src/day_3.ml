let dan = "3"

let data_to_list vsebina_datoteke = 
  let lines = String.split_on_char '\n' vsebina_datoteke in
  lines


let check_line char line i len =
  let index = i mod len in
  line.[index] = char


let naloga1 vsebina_datoteke =
  let lines = data_to_list vsebina_datoteke in 
  let rec aux count char lines i =
    match lines with
    	| [] -> count
    	| x :: xs ->(
				let len = String.length x in
				if (check_line char x i len) then aux (count + 1) char xs (i + 3)
				else aux count char xs (i + 3))
	in
	string_of_int (aux 0 '#' lines 0)

(* Pomožne funkcije za 2. nalogo *)    

let count_each_line lines jump =
	let rec aux count char lines i jump =
    match lines with
    	| [] -> count
    	| x :: xs ->(
				let len = String.length x in
				if (check_line char x i len) then aux (count + 1) char xs (i + jump) jump
				else aux count char xs (i + jump) jump)
	in
	aux 0 '#' lines 0 jump

	let count_each_second_line lines jump =
		let rec aux count countline char lines i jump = 
			match lines with 
			| [] -> count 
			| x :: xs ->
				let len = String.length x in
				if (countline mod 2 = 0) then (
					if check_line char x i len 
					then aux (count + 1) (countline + 1) char xs (i + jump) jump
					else aux count (countline + 1) char xs (i + jump) jump
					)
				else 
					if xs = [] then count
					else aux count (countline + 1) char xs i jump
		in
	aux 0 0 '#' lines 0 jump



let naloga2 vsebina_datoteke = 
  let lines = data_to_list vsebina_datoteke in 
	let one = count_each_line lines 1 in 
	let three = count_each_line lines 3 in 
	let five = count_each_line lines 5 in 
	let seven = count_each_line lines 7 in 
	let one_two = count_each_second_line lines 1 in
	string_of_int (one * three * five * seven * one_two)



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