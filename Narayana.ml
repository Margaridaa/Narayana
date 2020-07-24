open Printf
open Scanf

exception Invalid_input

let (n,k) = scanf " %d %d" (fun a b -> a,b)

(* Verify if n and k are valid *)
let raise_inv () = 
	if n < 1 || n > 30 || k < 1 || k > n  then raise Invalid_input
	else ()

let () = raise_inv ()

let rec binomial_coeff (n:int) (k:int) =
    if (k = 0 || k = n) then 1
    else (n * binomial_coeff (n-1) (k-1)) / k

let mountainsnumber (n:int) (k:int) = 	
	if k > n then failwith "mountainsnumber: Input Error"
	else (binomial_coeff (n:int) (k:int) * binomial_coeff (n:int) (k - 1) / n)


let () = printf "%d\n" (mountainsnumber n k)
