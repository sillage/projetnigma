#load "nums.cma"

open Big_int

let pprinter b = Format.print_string (string_of_big_int b);;
#install_printer pprinter

(*=============definition=================*)

let zero = zero_big_int
let un = unit_big_int
let plus x y = add_big_int x y
let moins x y = sub_big_int x y
let fois x y = mult_big_int x y
let dive x y = div_big_int x y
let mode x y = mod_big_int x y
let ofint x = big_int_of_int x
let eq x y = eq_big_int x y
let dif x y = not (eq_big_int x y)
let ofstring s = big_int_of_string s
let pow x y = power_big_int_positive_big_int x y
let pred x = pred_big_int x
let carre poulpe = square_big_int poulpe
let minus x = minus_big_int x
let pgcd x y = gcd_big_int x y
let succ x = succ_big_int x
let ofbig x = string_of_big_int x

(*==============fin definition===================*)


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
    ofstring(boucle "");;


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
    ofstring(boucle "")

let expo a n p =
  let rec expo2 accu x w = match w with
      n when (eq zero n) -> accu
    | n ->
        if eq (mode n (ofint 2)) zero then
	  expo2 accu (mode (fois x x) p) (dive n (ofint 2))
        else
	  expo2 (mode (fois accu x) p) (mode (fois x x) p) (dive n (ofint 2))
  in expo2 un a n

let calculb n =
  let rec calcul n b =
    if eq (mode n (ofint 2)) zero then
      calcul (dive n (ofint 2)) (plus b un)
    else
      b
  in calcul n zero;;

calculb (ofint 42);;

let miller_rabin n t =
  let b = calculb (pred n) in
  let r = (n-1)/(2**b) in (* This expression has type int but is here used with type float *)
    for i = 0 to t do
      (* FIXME Choisir un entier aléatoire témoin 1 < a < n - 1 FIXME *)
      let y = (a**r) mod n in
    if (y <> 1) && (y <> (n-1)) then
      let j = ref 1 in
        while (!j < b) && (y <> (n-1)) do
          y := !y**2 mod n; (* This expression has type int but is here used with type float *)
          if (y = 1) then false;
          j := !j+1;
          if (y <> (n-1)) then false;
        done;
	true
    else
      true
    done

let prime () =
  let p = ref (makenbprem ()) in
    while not (miller_rabin (!p) 5) do
      p := makenbprem()
    done;
    !p

let find_e n =
  let rec find n e =
    if eq (pgcd n e) un then
      e
    else
      find n (plus e un)
  in find n (ofint 3)

let phi p q = fois (pred p) (pred q)

let rec euclide a b =
  if eq b zero then (a,un,zero)
  else
    begin
      let (d',u',v') =
	euclide b (mode a b)
      in
	(d',v',moins u' (fois (dive a b) v'))
    end

let find_d e fi = match euclide e fi with
    (x,y,z) -> y


(*==================creation des cles======================*)
let make_key n e d =
  let public = open_out "cle_publique" in
  let prive = open_out "cle_prive" in
    output_string public ((ofbig n)^"\n"^(ofbig e));
    output_string prive ((ofbig n)^"\n"^(ofbig d));
    close_out public;
    close_out prive



(*==============conversion string => int  et vice-versa==================*)

let text_to_list x =
  let a = open_in x in
  let accu = in_channel_length a in
  let rec parc x = function
    | accu when accu = 1 -> []
    | accu -> x :: (parc (input_byte a) (accu-1))
  in
  let x = parc (input_byte a) (accu) in
    close_in a ;
    x

let int_list_to_text l file =
  let a = open_out file in
  let rec parc = function
      [] -> ""
    | e::l -> string_of_int(e)^";"^(parc l)
  in
    output_string a (parc l);
    close_out a

let big_int_list_to_text l file =
  let a = open_out file in
  let rec parc = function
      [] -> ""
    | e::l -> ofbig(e)^";"^(parc l)
  in
    output_string a (parc l);
    close_out a


let rec cryptlist l e n = match l with
    [] -> []
  | h::t -> (expo (ofint h) e n)::(cryptlist t e n)

let textlist_to_list file =
  let a = open_in file in
  let s = input_line a in
  let rec decode i s accu =
    if (i = String.length s) then
      []
    else
      match s.[i] with
	| c when c = ';' -> accu::decode (i+1) s zero
	| c -> decode (i+1) s (plus (fois accu (ofint 10))
			      (ofint((int_of_char c) - (int_of_char '0'))))
  in
  let l = decode 0 s zero in
    close_in a;
    l


let crypt file n e =
  let list = text_to_list file in
  let crypt_list = cryptlist list e n in
    big_int_list_to_text crypt_list (file^"_crypter")


let decrypt file n d =
  let cryptlist = textlist_to_list file in
  let rec decode = function
      [] -> []
    | h::t -> (expo h d n)::decode t
  in
  let newlist = decode cryptlist in
    big_int_list_to_text newlist (file^"_decrypter");;

crypt "teste.txt" (ofint 61779551) (ofint 5)  ;;
decrypt "teste.txt_crypter"  (ofint 61779551) (ofint 24705533);;


(*========================================================*)

let main () =
  let p = ofint(7853) in
  let q = ofint(7867) in
  let n = fois p q in
  let e = find_e (phi p q) in
  let d = find_d e (phi p q) in
    make_key n e d

let _ = main()
