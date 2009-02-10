(*#load "nums.cma";;
let pprinter b = Format.print_string (Big_int.string_of_big_int b);;
#install_printer pprinter*)

(*=============definition=================*)

let zero = Big_int.zero_big_int
let un = Big_int.unit_big_int
let plus x y = Big_int.add_big_int x y
let moins x y = Big_int.sub_big_int x y
let fois x y = Big_int.mult_big_int x y
let dive x y = Big_int.div_big_int x y
let mode x y = Big_int.mod_big_int x y
let ofint x = Big_int.big_int_of_int x
let eq x y = Big_int.eq_big_int x y
let dif x y = not (Big_int.eq_big_int x y)
let ofstring s = Big_int.big_int_of_string s
let pow x y = Big_int.power_big_int_positive_big_int x y
let pred x = Big_int.pred_big_int x
let carre poulpe = Big_int.square_big_int poulpe
let minus x = Big_int.minus_big_int x
let pgcd x y = Big_int.gcd_big_int x y
let succ x = Big_int.succ_big_int x
let int_of_big x = Big_int.int_of_big_int x
let ofbig x = Big_int.string_of_big_int x

(*==============Creation nombres randoms===================*)


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

let expo a n p = (*===exponentiation modulaire===*)
  let rec expo2 accu x w = match w with
      n when (eq zero n) -> accu
    | n ->
        if eq (mode n (ofint 2)) zero then
	  expo2 accu (mode (fois x x) p) (dive n (ofint 2))
        else
	  expo2 (mode (fois accu x) p) (mode (fois x x) p) (dive n (ofint 2))
  in expo2 un a n

(*===================liste des nombres premiers <= 1024=======================*)
let littleprimenumbers =
  [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67;
   71; 73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149;
   151; 157; 163; 167; 173; 179; 181; 191; 193; 197; 199; 211; 223; 227; 229;
   233; 239; 241; 251; 257; 263; 269; 271; 277; 281; 283; 293; 307; 311; 313;
   317; 331; 337; 347; 349; 353; 359; 367; 373; 379; 383; 389; 397; 401; 409;
   419; 421; 431; 433; 439; 443; 449; 457; 461; 463; 467; 479; 487; 491; 499;
   503; 509; 521; 523; 541; 547; 557; 563; 569; 571; 577; 587; 593; 599; 601;
   607; 613; 617; 619; 631; 641; 643; 647; 653; 659; 661; 673; 677; 683; 691;
   701; 709; 719; 727; 733; 739; 743; 751; 757; 761; 769; 773; 787; 797; 809;
   811; 821; 823; 827; 829; 839; 853; 857; 859; 863; 877; 881; 883; 887; 907;
   911; 919; 929; 937; 941; 947; 953; 967; 971; 977; 983; 991; 997; 1009;
   1013; 1019; 1021]

(*=============Test de primalité probabiliste de Miller-Rabin=================*)
let _millerRabin a n =
  (* Ne pas appeler directement (fonction utilitaire).
     Appeler millerRabin(n, k=20) *)
  (* trouver s et d pour transformer n-1 en (2**s)*d *)
  let d = ref (pred n) in
  let s = ref 0 in
  let res = ref false in
  let r = ref 0 in
    while eq (mode !d (ofint 2)) zero do
      d := dive !d (ofint 2);
      s := !s + 1;
    done;
    (* calculer l'exponentiation modulaire (a**d)%n *)
    let apow = expo a !d n in (* =(a**d)%n *)
      (* si (a**d)%n == 1 => n est probablement 1er *)
      if eq apow un then
	res := true
      else
	while (!r <= !s) && (not !res) do 
	  (* si a**(d*(2**r))%n == (n-1) => n est probablement 1er *)
          if eq (expo a !d n) (pred n) then res := true;
	  d := fois !d (ofint 2);
	  r := !r + 1;
	done;
      !res

let millerRabin n k = (* choisir k=20 *)
  (* Test de primalité probabiliste de Miller-Rabin *)
  let res = ref true in
  let a = ref un in
  let repete = ref 0 in
    (* éliminer le cas des multiples des petits nombres <=1024 *)
    if Big_int.le_big_int n (ofint 1024) then
      res := List.exists
	(fun x -> eq (mode n (ofint x)) zero)
	littleprimenumbers
    else
      (* éliminer le cas des nombres pairs qui ne peuvent pas Ãªtre 1ers! *)
      if eq (mode n (ofint 2)) zero then
	res := false
      else
	(* recommencer le test k fois: seul les nb ayant réussi k fois
	   seront True *)
	  while (!repete <= k) && (!res) do
	    (*for repete = 0 to k do*)
	    (* trouver un nombre au hasard entre 1 et n-1 (bornes inclues) *)
	    a := makenba ();
	    (* si le test echoue une seule fois => n est composé *)
	    if not(_millerRabin !a n) then res := false;
	    (* n a réussi les k tests => il est probablement 1er *)
	    repete := !repete + 1;
	  done;
    !res;;



(*===element pour la creation des clefs privee et publique===*)

let prime () =
  let p = ref (makenbprem ()) in
    while not (millerRabin (!p) 20)do
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
    output_string public ((ofbig n)^"\n"^(ofbig e)^"\n");
    output_string prive ((ofbig n)^"\n"^(ofbig d)^"\n");
    close_out public;
    close_out prive



(*==============conversion fichier <=> liste==================*)

(*prend un fichier texte et cree une liste avec les codes ascii de chaque
caractere du fichier texte*)
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

(*prend une int list et cree le fichier representant cette liste*)
let int_list_to_text l file =
  let a = open_out file in
  let rec parc = function
      [] -> ""
    | e::l -> string_of_int(e)^";"^(parc l)
  in
    output_string a (parc l);
    close_out a

(*prend une bigint list et cree le fichier representant cette liste*)
let big_int_list_to_text l file =
  let a = open_out file in
  let rec parc = function
      [] -> ""
    | e::l -> ofbig(e)^";"^(parc l)
  in
    output_string a (parc l);
    close_out a

(*crypte chaque elt de la liste en utilisant la cle publique*)
let cryptlist l cle_publique =
  let a = open_in cle_publique in
  let n = ofstring(input_line a) in
  let e = ofstring(input_line a) in
  let rec crypte l e n =
    match l with
	[] -> []
      | h::t -> (expo (ofint h) e n)::(crypte t e n)
  in
  let list = crypte l e n in
    close_in a;
    list

let decryptlist l cle_prive =
  let a = open_in cle_prive in
  let n = ofstring(input_line a) in
  let d = ofstring(input_line a) in
  let rec crypte l e n =
    match l with
	[] -> []
      | h::t -> (expo h d n)::(crypte t d n)
  in
  let list = crypte l d n in
    close_in a;
    list


(*convertit une liste dans un fichier en un liste Ocaml*)
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

let textlist_to_list_int file =
  let a = open_in file in
  let s = input_line a in
  let rec decode i s accu =
    if (i = String.length s) then
      []
    else
      match s.[i] with
	| c when c = ';' -> accu::decode (i+1) s 0
	| c -> decode (i+1) s ((accu * 10)+
			      ((int_of_char c) - (int_of_char '0')))
  in
  let l = decode 0 s 0 in
    close_in a;
    l


(*====crypte/decrypte les fichier a l'aide des cles====*)

(*crypte un fichier a l'aide de la cle publique*)
let crypt file cle_publique =
  let list = text_to_list file in
  let crypt_list = cryptlist list cle_publique in
    big_int_list_to_text crypt_list (file^"_crypter")

(*decrypte un fichier a l'aide de la cle privee*)
let decrypt file cle_prive =
  let liste_cryptee = textlist_to_list file in
  let newlist = decryptlist liste_cryptee cle_prive in
  let a = open_out (file^"_decrypter") in
  let rec ascii_to_text chanel = function
      [] -> close_out chanel
    | e::l -> output_char chanel (char_of_int(int_of_big e));
	ascii_to_text chanel l
  in
    ascii_to_text a newlist

(* big_int_list_to_text newlist (file^"_decrypter") in*)
(*crypt "test.txt" "cle_publique"  ;;
decrypt "test.txt_crypter"  "cle_prive";;*)


(*========================================================*)

let fabrique_cle () =
  let p = prime ()(*ofint(961748927)*) in
  let q = prime ()(*ofint(982451653)*) in
  let n = fois p q in
  let e = find_e (phi p q) in
  let d = find_d e (phi p q) in
    make_key n e d

let main() =
  if Array.length (Sys.argv) < 2 then
    Printf.printf("Entree incorrecte : \n
tapez ./solovay --help pour plus d'info\n")
  else
    begin
      match Sys.argv.(1) with
	  x when x = "--help" ->
	    begin
	      Printf.printf("menu d'aide :\n");
	      Printf.printf("--help : Affiche le menu d'aide\n");
	      Printf.printf("--clefs : Cree aleatoirement une cle");
	      Printf.printf(" publique et sa cle privee correspondante\n");
	      Printf.printf("--crypte : Chiffre le fichier dont le nom est");
	      Printf.printf(" place en deuxieme parametre\n");
	      Printf.printf("--decrypte : Dechiffre le fichier dont le");
	      Printf.printf(" nom est place en deuxieme parametre\n");
	    end
	| x when x = "--clefs" ->
	    begin
	      fabrique_cle();
	      Printf.printf("clefs fabriquees !\n");
	    end
	| x when x = "--crypte" ->
	    begin
	      if (Array.length (Sys.argv) = 2) ||
		(not(Sys.file_exists Sys.argv.(2))) then
		  begin
		    Printf.printf("nom de fichier a crypter manquant");
		    Printf.printf (" ou incorrect\n");
		  end
	      else
		if (Sys.file_exists "cle_publique") then
		  begin
		    crypt Sys.argv.(2) "cle_publique";
		    Printf.printf("Cryptage reussi!\n");
		  end
		else
		  Printf.printf("Erreur : clefs non generees...\n")
	    end
	| x when x = "--decrypte" ->
	    begin
	      begin
		if Array.length (Sys.argv) = 2 then
		  Printf.printf("nom de fichier a decrypter manquant\n")
		else
		  if (Sys.file_exists "cle_prive") then
		    begin
		      decrypt Sys.argv.(2) "cle_prive";
		      Printf.printf("Decryptage reussi!\n");
		    end
		  else
		    Printf.printf("Erreur : clefs non generees...\n")
	      end
	    end
	| _ -> Printf.printf("Entree incorrecte : \n
tapez ./solovay --help pour plus d'info\n")
    end

let _ = main()
