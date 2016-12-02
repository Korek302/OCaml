type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let rec put = function
	(value, Node (v, l, r)) -> 
		if value < v then Node(v, put(value,l), r) 
		else if value > v then Node(v, l, put(value,r)) 
		else failwith "duplicated key"
	| (value, Empty) -> Node(value, Empty, Empty)
;;

let rec english(bt, polish) =
	match bt with
		Node(v, l, r) -> 	if fst v > polish then english(l, polish)
					else if fst v < polish then english(r, polish)
					else snd v
		|Empty -> failwith "no such word in dictionary";;

let interface()=
	let rec temp(dictionary)=
		print_string("1. Dodaj slowo do slownika\n2. Wyszukaj slowo w slowniku\n");
		let choice = read_line() in
		if choice = "1;;" then
			(
			print_string("Podaj slowo po polsku\n");
			let pol = read_line() in print_endline("");
			print_string("Podaj znaczenie po angielsku\n");
			let eng = read_line() in print_endline("");
			let dictionary = put((pol, eng), dictionary) in
			print_endline("");
			temp(dictionary)
			)
		else if choice = "2;;" then
			(
			print_string("Podaj slowo po polsku\n");
			let pol = read_line() in print_endline("");
			print_string(english(dictionary, pol));
			print_endline("");
			temp(dictionary);
			print_endline("");
			)
		else 
			(
			print_string "Zakonczono dzialanie programu "; 
			print_endline("");
			)
		in temp(Empty)
		;;
