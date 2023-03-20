let  hd l=
 match l with
  | []-> raise (Failure "hd")
  | h::t->h;;

let  tl l=
 match l with
  | []-> raise (Failure "tl")
  | h::t-> t;;

let rec append a b=
 match a with 
  | []->b
  | h::t->h::append t b;;

let rec rev l=
    match l with
    | []->[]
    | h::t->append (rev t) [h];;

(*_______________________________________________________________*)

let rec remove x l=
    match l with
    | []->[]
    | h::t->if h=x then t 
                else h::(remove x t);;

let rec remove_all x l=
    match l with 
    | []->[]
    | h::t-> if h=x then remove_all x t
                else h::remove_all x t;;

let rec ldif l1 l2=
    match l2 with 
    | []->l1
    | h::t->ldif (remove_all h l1) t;; 

let rec lprod l1 l2=
	let rec aux cnt = function
		  [],_ -> rev cnt
		| _::t1,[] -> aux cnt (t1,l2)
		| h1::t1,h2::t2 -> aux ((h1,h2)::cnt) (h1::t1,t2)
	in aux [] (l1,l2);;

let rec divide l=
    match l with
    | []->([],[])
    | h1::h2::t->let (x,y)= divide t in (h1::x, h2::y)
    | l->(l, []);;

