let rec sortQ = function
    | [] -> []
    | x :: l -> insert x (sortQ l)
  and insert elem = function
    | [] -> [elem]
    | x :: l -> if snd elem < snd x then elem :: x :: l
                else x :: insert elem l;;

module type PRIORITY_QUEUE =
	sig
		type 'a t
		exception Empty of string
		exception Full of string
		val size: int
		val create: unit -> 'a t
		val enqueue: ('a * int) * 'a t -> 'a t
		val top: 'a t -> 'a * int
		val pop: 'a t -> 'a t
		val merge: 'a t * 'a t -> 'a t
		val print: 'a t -> ('a * int) list * int
	end;;
		
module Queue : PRIORITY_QUEUE =
	struct
		type 'a t = EmptyQueue | Q of ('a * int) list * int
		exception Empty of string
		exception Full of string
		
		let size = 10
		
		let create() = EmptyQueue
		
		let rec enqueue(e, q) = 
			match q with
			| EmptyQueue -> Q([e], size - 1)
			| Q(list, space) -> if space <= 0 then raise (Full "module Queue: enqueue")
													else Q(sortQ(e::list), space - 1)
		
		let top = function
			| Q([], _) -> raise (Empty "module Queue: top")
			| Q(h::t, _) -> h
			| EmptyQueue -> raise (Empty "module Queue: top")

		let pop = function
			| Q([], _) -> raise (Empty "module Queue: pop")
			| Q(h::t, space) -> if t = [] then EmptyQueue 
													else Q(t, space + 1)
			| EmptyQueue -> raise (Empty "module Queue: pop")

		let rec merge(que1, que2)=
			match (que1, que2) with
			| (EmptyQueue, _) -> que2
			| (_, EmptyQueue) -> que1
			| (Q(list1, space1), Q(list2, space2)) -> if space1+space2 < size then raise (Full "module Queue: merge")
																								else Q(sortQ(list1@list2), space1+space2-size)

		let rec print = function
			| Q(list, space) -> (list, space)
			| EmptyQueue -> ([], 10)
	end;;

let q = Queue.enqueue(('k', 5), Queue.create());;
let q = Queue.enqueue(('f', 1), q);;
let q = Queue.enqueue(('g', 7), q);;
let q = Queue.enqueue(('h', 2), q);;
let q = Queue.enqueue(('q', 2), q);;
let q = Queue.enqueue(('e', -1), q);;
Queue.print q;;
Queue.top q;;
Queue.print(Queue.pop q);;
let q0 = Queue.enqueue(('t', 6), Queue.enqueue(('d', 17), Queue.create()));;
Queue.print q0;;
Queue.print(Queue.merge(q, q0));;