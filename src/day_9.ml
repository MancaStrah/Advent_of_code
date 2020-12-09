let dan = "9"

let data_to_list vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke

let data_to_int_list vsebina_datoteke = List.map int_of_string (data_to_list vsebina_datoteke)

let rec sort = function
   | [] -> []
   | x :: l -> insert x (sort l)
 and insert elem = function
   | [] -> [elem]
   | x :: l -> if elem < x then elem :: x :: l
               else x :: insert elem l
(* From https://ocaml.org/learn/taste.html#Polymorphism-sorting-lists *)

let rec takefirstk k xs = match xs with
| [] -> failwith "firstk"
| x::xs -> if k=1 then [x] else x::takefirstk (k-1) xs
(* From https://stackoverflow.com/questions/26543669/ocaml-return-first-n-elements-of-a-list *)


let check_list list25 element26 =  
  let rec aux sorted element26 =
    match sorted with
    | [] -> (false, element26)
    | x :: xs -> (
      if List.mem (element26 - x) sorted then (true, element26)
      else (if x > (element26 / 2 ) then (false, element26) else aux xs element26)
    )
  in
  let sorted = (sort list25) in
  aux sorted element26

let check_all int_list =
  let rec aux int_list = 
    match int_list with 
    | x :: xs -> (
      (* To zdej je zelo grdo. *)
      let first_26 = x :: takefirstk 25 xs in 
      let first_25 = takefirstk 25 first_26 in 
      let number_26_list = takefirstk 1 (List.rev first_26) in 
      let number_26 = match number_26_list with | x :: [] -> x | _ -> failwith "Ojoj number 26" in 
      let (is_true, number) = check_list first_25 number_26 in 
      if is_true then aux xs else number_26
    )
    | _ -> failwith "ojoj,checkall"
  in 
  aux int_list

let naloga1 vsebina_datoteke =
  vsebina_datoteke |> data_to_int_list |> check_all |> string_of_int

(* Pomožne funkcije za 2. nalogo. *)

let my_number = 776203571 

let check_list list my_number = 
  let rec aux sum  sum_list list my_number =
    match list with
    | [] -> (false, 0)  
    | x :: xs -> (
      let new_sum = sum + x in 
      if new_sum < my_number then aux new_sum (x :: sum_list) xs my_number else 
      (if new_sum = my_number then (
        let sorted_sum = sort sum_list in
        let smallest = List.nth sorted_sum 0 in 
        let sorted_reversed = List.rev sorted_sum in 
        let largest = List.nth sorted_reversed 0 in 
        (true, largest + smallest)
      )
      else (false, 0))
    )
  in
  aux 0 [] list my_number

let check_all2 int_list  =
  let length = List.length int_list in 
  let rec aux i int_list my_number length =
    (* Elegantno ...  *)
    let ok_list = List.rev (takefirstk (length - i) (List.rev int_list)) in 
    let (is_true, number) = check_list ok_list my_number in 
    if is_true then number else aux (i + 1) int_list my_number length 
  in 
  aux 0 int_list 776203571 length

let naloga2 vsebina_datoteke =
  vsebina_datoteke |> data_to_int_list |> check_all2 |> string_of_int

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