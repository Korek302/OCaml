type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let rec put = function
	(value, Node (v, l, r)) -> 
		if value < v then Node(v, put(value,l), r) 
		else if value > v then Node(v, l, put(value,r)) 
		else failwith "duplicated key"
	| (value, Empty) -> Node(value, Empty, Empty)
;;

let slownik = Empty;;
let slownik = put (("mleko","milk"), slownik);;
let slownik = put (("kot","cat"), slownik);;
let slownik = put (("pies","dog"), slownik);;
let slownik = put (("auto","car"), slownik);;
let slownik = put (("kon","horse"), slownik);;
let slownik = put (("aparat","camera"), slownik);;
let slownik = put (("koto","koto"), slownik);;

	
let rec english(bt, polish) =
	match bt with
		Node(v, l, r) -> 	if fst v > polish then english(l, polish)
					else if fst v < polish then english(r, polish)
					else snd v
		|Empty -> failwith "no such word in dictionary"
;;

english(slownik, "kot");;
english(slownik, "kon");;
english(slownik, "pies");;
english(put(("ryba","fish"),slownik), "ryba");;
let slownik = put(("ryba","fish"),slownik);;

let add() =
	print_string "Podaj angielskie znaczenie \n";
	let ang = read_line() in
	print_string "Podaj polskie znaczenie \n";
	let pl = read_line() in
	put((pl, ang), slownik);;
	

let search =
	print_string "Czego wyszukac? \n";
	let value = read_line() in
	english(slownik, value);;

let interface choice = let choice = read_line() in
	begin
	print_string "1. Dodaj do slownika \n";
	print_string "2. Wyszukaj w slowniku \n";
	
	while choice <> "0" do
		match choice with
		| "1" -> add()
		| "2" -> search()
		
		choice = read_line();
		done;
		print_newline()
		end;;