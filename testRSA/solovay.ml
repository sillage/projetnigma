#load "nums.cma";;

open Big_int;;

let pprinter b = Format.print_string (string_of_big_int b);;
#install_printer pprinter;;

let randome () = Random.bits();;

let makenbprem () =
  Random.self_init();
  let rec boucle s =
    match String.length s with
	x when x>500 ->""
      | x ->begin
	  let a =string_of_int(randome())
	  in
	    a^boucle(s^a)
	end
  in
    boucle "";;

let makenba () =
  Random.self_init();
  let rec boucle s =
    match String.length s with
	x when x>470 ->""
      | x ->begin
	  let a =string_of_int(randome())
	  in
	    a^boucle(s^a)
	end
  in
    boucle "";;

ofstring (makenba());;

let zero = zero_big_int ;;
let un = unit_big_int ;;
let fois x y = mult_big_int x y ;;
let dive x y = div_big_int x y ;;
let mode x y = mod_big_int x y ;;
let ofint x = big_int_of_int x ;;
let eq x y = eq_big_int x y ;;
let dif x y = not (eq_big_int x y) ;;
let ofstring s = big_int_of_string s;;
let pow x y = power_big_int_positive_big_int x y ;;
let pred x = pred_big_int x ;;

let expo a n p =
  let rec expo2 accu x w = match w with
      n when (eq zero n) -> accu
    | n ->
        if eq (mode n (ofint 2)) zero then
	  expo2 accu (mode (fois x x) p) (dive n (ofint 2))
        else
	  expo2 (mode (fois accu x) p) (mode (fois x x) p) (dive n (ofint 2))
  in expo2 un a n;;

expo (ofstring (makenba())) (ofint 13) (ofint 12);;

let tail_expo_modulaire_2 a n p =
  let rec expo accu x w = match w with
    | 0 -> accu
    | n ->
        if n mod 2 = 0
        then expo accu ((x * x) mod p) (n / 2)
        else expo ((accu * x ) mod p) ((x * x) mod p) (n / 2)
  in expo 1 a n;;

tail_expo_modulaire_2 4 13 12;;

(*---------------marche pas encore...----------------------*)

let rec solovay n = function
    0 -> true
  | k ->
      begin
	(*Random.self_init();*)
	if (eq (mode n (ofint 2)) zero) then
	  false
	else
	  begin
	    let a = ofstring (makenba()) in
	    let x = mode a n in
	      if (eq x zero) or dif (expo a (dive (pred n) (ofint 2)) n) x then
		false
	      else
		solovay n (k-1)
	  end
      end;;

solovay (ofstring(makenbprem())) 100 ;;
solovaytest (ofint(1933)) 100 ;;

let rec solovaytest n = function
    0 -> true
  | k ->
      begin
	if (eq (mode n (ofint 2)) zero) then
	  false
	else
	  begin
	    Random.self_init();
	    let a = ofint((Random.int 499)+1) in
	    let x = mode a n in
	      if (eq x zero) or dif (expo a (dive (pred n) (ofint 2)) n) x then
		false
	      else
		solovay n (k-1)
	  end
      end;;
