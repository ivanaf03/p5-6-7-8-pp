let  hd l=
 match l with
  | []-> raise (Failure "hd")
  | h::t->h;;

let  tl l=
 match l with
  | []-> raise (Failure "tl")
  | h::t-> t;;

let rec length l=
 match l with
  | []->0
  | h::t->1+length t;;

let compare_lengths l1 l2=
    if length l1= length l2 then 0
    else if length l1>length l2 then 1 else -1;;

let rec nth l1 n=
 match l1 with
  | []-> raise (Failure "nth")
  | h::t->if n=0 then h
              else if n<0 then raise (Invalid_argument "n<0")
                      else nth t (n-1);;

let rec append a b=
 match a with 
  | []->b
  | h::t->h::append t b;;

let rec find f l=
 match l with
  | []-> raise (Not_found)
  | h::t->if f h=true then h
             else find f t;;
             
let rec for_all f l=
 match l with
  | []->true
  | h::t->if f h=true then (for_all f t)
              else false;;

let rec exists f l=
 match l with
  | []->true
  | h::t->if f h=false then (for_all f t)
              else true;;

let rec mem a l=
    match l with
    | []->false
    | h::t->if a=hd l then true     
                else mem a t;;

let rec filter f l=
    match l with
    | []->[]
    | h::t->if f h=true then h::filter f t
                else filter f t;;

let rec find_all f l=
    match l with
    | []->[]
    | h::t->if f h=true then h::find_all f t
                else find_all f t;;
    
let rec partition f l=
    match l with
    | []->([], [])
    | h::t->let (x,y)= partition f t in 
        if f h then (h::x, y)
        else (x, h::y);;

let rec split l=
    match l with
    | []->([],[])
    | (h1,h2)::t-> let (t1,t2)=split t in
        (h1::t1), (h2::t2);;
        
let rec combine l1 l2 =
    match (l1,l2) with
        | [], [] -> []
        | h1::t1, h2::t2 -> (h1,h2) :: (combine (t1) (t2))
        | _ -> raise (Invalid_argument "combine");;

let rec rev l=
    match l with
    | []->[]
    | h::t->append (rev t) [h];;

let  init len f=
    if len<0 then raise(Invalid_argument "init")
    else let rec init_in l acc=
            if acc=len then rev l
            else init_in (f acc::l) (acc+1)
        in init_in [] 0;;

let rev_append l1 l2=
    append (rev l1) l2;;

let rec concat l=
    match l with
    | []->[]
    | h::t->append h (concat t);;

let rec flatten l=
    match l with
    | []->[]
    | h::t->append h (flatten t);;

let rec map f l=
    match l with
    | []->[]
    | h::t->f h::map  f t;;

let rev_map f l=
    rev(map f l);;

let rec map2 f l1 l2 =
    if length l1 <> length l2 then raise (Invalid_argument"map2")
    else if length l1 == 0 then []
            else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2);;

let rec fold_left f x l=
    match l with
    | [] -> x
    | h::t -> fold_left f (f x h) t;;


let rec fold_right f l x =
    match l with
    | [] -> x
    | h::t -> f h (fold_right f t x);;























  






