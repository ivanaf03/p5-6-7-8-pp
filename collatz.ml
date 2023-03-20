let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec orbit n=
  match n with
    | 0-> raise(Failure("orbit"))
    | 1->"1"
    | _->string_of_int(n)^","^orbit (f n);;

let rec length n=
  match n with 
    | 0->raise(Failure("length"))
    | 1->0
    | _->1+length(f n);;

let rec top n=
  if n<=0 then raise(Failure("top")) 
  else if n=1 then 0
          else max n (top(f n));;

let rec length'n'top n= 
  match n with
  | 1-> (0,0)
  | _-> let x = length'n'top (f n) in (1 + fst x , max n (snd x));;

let rec longest_in m n=
    if m<=n then if (length m>=length(n)) then  longest_in m (n-1) 
                          else longest_in (m+1)  n
                   else (m, length m);;

let rec highest_in m n=
    if m<=n then if (top m>=top(n)) then  highest_in m (n-1) 
                          else highest_in (m+1)  n
                   else (m, top m);;


