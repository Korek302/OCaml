let swap tab i j =
	let temp = tab.(i) 
	in 
	tab.(i) <- tab.(j);
	tab.(j) <- temp;;

let bubbleSort tab = 
	begin
		for i = 1 to Array.length tab -1 do
			for j = 0 to Array.length tab - i - 1 do
				if tab.(j) > tab.(j+1) then swap tab (j+1) j done;
		done;
	end;;
				
let t1 = [|4; 8; 1; 12; 7; 3; 1; 9|];;
bubbleSort t1;;
t1;;