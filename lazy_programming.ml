(* ---------- Hamming Numbers ----------- *)

let rec merge s1 s2 =
  { hd = if s1.hd<s2.hd then s1.hd else s2.hd;
    tl = Susp (fun () -> if s1.hd=s2.hd then merge (force s1.tl) (force s2.tl) 
                else if s1.hd<s2.hd then merge (force s1.tl) s2 
                else merge s1 (force s2.tl)) }

let rec hamming_series =
  { hd = 1;
    tl = Susp (fun () -> merge (times 2 hamming_series) 
                  (merge (times 3 hamming_series) (times 5 hamming_series))) }


type 'a susp = Susp of (unit -> 'a)
                       
let force (Susp f) = f ()
    
type 'a stream = { hd : 'a ; 
                   tl : ('a stream) susp }
              
(*define stream of ones : 1, 1, 1...*)
let rec ones = { hd =1 ;
                 tl = Susp (fun () -> ones) }
               
(*take : int -> 'a str -> 'a list*)
let rec take n str = if n = 0 then []
  else str.hd :: take (n-1) (force str.tl)
         
let rec numsFrom n = {hd = n ; tl = Susp (fun () -> numsFrom (n+1))}
  
let rec evensFrom n = {hd = n ; tl = Susp (fun () -> evensFrom (n+2))}
                      
let rec stepNums n k = {hd=n; tl=Susp (fun () -> stepNums (n+k) k)}
                       
let evens = stepNums 0 2

(* ---------- Power Series ----------- *)
(*Example: power series: Compute the power series such that 
if k=2, we get 1, 2, 4, 8, 16...
if k=3, we get 1, 3, 9, 27, 81...*)
let rec power n k = if k=0 then 1 else n * power n (k-1)
let rec power_series n k = 
  {hd = n; tl = Susp (fun () -> power_series (n*k) k)}
  
let ps2 = power_series 1 2
(*
  {hd = 1; tl = Susp (fun () -> power_series 2 2)}.tl (force the tail)
  {hd = 2; tl = Susp (fun () -> power_series 4 2)}
*)

(*add : int str -> int str -> int str*)

let rec add str1 str2= 
  {hd = str1.hd + str2.hd; 
   tl =  Susp (fun ()-> add (force str1.tl) (force str2.tl))}

let rec smap f s =
  { hd = f s.hd;
    tl = Susp (fun () -> smap f (force s.tl)) (*black box*)
  }
  
(*find : ('a -> bool) -> 'a str -> 'a * 'a str*)
(*first element satisfies the predicate and return it together with the tail*)
let rec find p s =
  if p s.hd then (s.hd, force s.tl)
  else find p (force s.tl)
      
(*filter : ('a -> bool) -> 'a str -> 'a str*)
(*filter should take all things in a stream that satisfy the predicate.*)

let rec sfilter p s = 
  if p s.hd then {hd = s.hd; tl = Susp (fun () -> sfilter p (force s.tl))}
  else sfilter p (force s.tl)
      
let rec sfilter' p s = 
  let (h, t) = find p s in
  {hd = h;
   tl = Susp (fun () -> sfilter p t) }

(* ---------- Sieve Method to Find All Prime Numbers ----------- *)
(*find might be useful
sieve of ertosthenes for prime numbers
idea: filter out each successive sequence of multiples*) 
let not_divider m n = not (n mod m = 0)
let rec sieve s = {
  hd = s.hd;
  tl = Susp (fun () -> sieve (sfilter (fun n -> not_divider s.hd n) (force s.tl)))
    (*recurse with sielve where you have filtered out all of the multiples
    of s.hd from the tail of s*)
}
