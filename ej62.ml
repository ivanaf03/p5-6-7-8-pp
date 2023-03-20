let uncurry f(a,b)=f a b;;

let curry f a b=f(a,b);;

uncurry (+);;
(* - : int * int -> int = <fun> *)

let sum=(uncurry(+));;
(* val sum : int * int -> int = <fun> *)

(* sum 1;; *)
(* This expression has type int but an expression was expected of type int * int *)

sum(2,1);;
(* - : int = 3 *)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(* val g : int -> int -> int = <fun> *)

(* g (2,5);; *)
(* This expression has type 'a * 'b but an expression was expected of type int *)

let h= g 2;;
(* val h : int -> int = <fun> *)

(* h1, h2, h3;; *)
(*  - : int * int * int = (7, 10, 13) *)

let comp f g x=f(g(x));;
(* val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)

let f = let square x = x * x in comp square ((+) 1);;
(* val f : int -> int = <fun> *)

(* f1, f2, f3;; *)
(* - : int * int * int = (4, 9, 16) *)

let i=function a->a;;
(* val i : 'a -> 'a = <fun> *)

let j=function (a,b)->a;;
(*  val j : 'a * 'b -> 'a = <fun> *)

let k=function(a,b)->b;;
(*  val k : 'a * 'b -> 'b = <fun> *)

let l=function a->[a];;
(* val l : 'a -> 'a list = <fun> *)
