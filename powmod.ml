let rec power' x y= if y<=0 then 1  else  if y mod 2=0 then power' (x*x) (y/2 ) else  x* (power' (x*x) (y/2 ));;

(* let powmod m b e= if b<=0 then 0 else if e<=0 then 1  else if (power' (m-1) 2)<=max_int
    then ((power' b e mod m)+m) mod m else 0;; *)
(* Definición de powmod para valores pequeños *)

let rec powmod m b e= if b<=0 then 0 
                                    else if e<=0 then 1 
                                            else ((b mod m)*(powmod m b (e-1))) mod m;;
(* Definición correcta para casi cualquier valor*)
