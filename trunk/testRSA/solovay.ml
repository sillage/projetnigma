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
  [2; 3; 5; 7; 11; 13; 17; 19;
   23; 29; 31; 37; 41; 43; 47; 53;
   59; 61; 67; 71; 73; 79; 83; 89;
   97; 101; 103; 107; 109; 113; 127; 131;
   137; 139; 149; 151; 157; 163; 167; 173;
   179; 181; 191; 193; 197; 199; 211; 223;
   227; 229; 233; 239; 241; 251; 257; 263;
   269; 271; 277; 281; 283; 293; 307; 311;
   313; 317; 331; 337; 347; 349; 353; 359;
   367; 373; 379; 383; 389; 397; 401; 409;
   419; 421; 431; 433; 439; 443; 449; 457;
   461; 463; 467; 479; 487; 491; 499; 503;
   509; 521; 523; 541; 547; 557; 563; 569;
   571; 577; 587; 593; 599; 601; 607; 613;
   617; 619; 631; 641; 643; 647; 653; 659;
   661; 673; 677; 683; 691; 701; 709; 719;
   727; 733; 739; 743; 751; 757; 761; 769;
   773; 787; 797; 809; 811; 821; 823; 827;
   829; 839; 853; 857; 859; 863; 877; 881;
   883; 887; 907; 911; 919; 929; 937; 941;
   947; 953; 967; 971; 977; 983; 991; 997;
   1009; 1013; 1019; 1021; 1031; 1033; 1039; 1049;
   1051; 1061; 1063; 1069; 1087; 1091; 1093; 1097;
   1103; 1109; 1117; 1123; 1129; 1151; 1153; 1163;
   1171; 1181; 1187; 1193; 1201; 1213; 1217; 1223;
   1229; 1231; 1237; 1249; 1259; 1277; 1279; 1283;
   1289; 1291; 1297; 1301; 1303; 1307; 1319; 1321;
   1327; 1361; 1367; 1373; 1381; 1399; 1409; 1423;
   1427; 1429; 1433; 1439; 1447; 1451; 1453; 1459;
   1471; 1481; 1483; 1487; 1489; 1493; 1499; 1511;
   1523; 1531; 1543; 1549; 1553; 1559; 1567; 1571;
   1579; 1583; 1597; 1601; 1607; 1609; 1613; 1619;
   1621; 1627; 1637; 1657; 1663; 1667; 1669; 1693;
   1697; 1699; 1709; 1721; 1723; 1733; 1741; 1747;
   1753; 1759; 1777; 1783; 1787; 1789; 1801; 1811;
   1823; 1831; 1847; 1861; 1867; 1871; 1873; 1877;
   1879; 1889; 1901; 1907; 1913; 1931; 1933; 1949;
   1951; 1973; 1979; 1987; 1993; 1997; 1999; 2003;
   2011; 2017; 2027; 2029; 2039; 2053; 2063; 2069;
   2081; 2083; 2087; 2089; 2099; 2111; 2113; 2129;
   2131; 2137; 2141; 2143; 2153; 2161; 2179; 2203;
   2207; 2213; 2221; 2237; 2239; 2243; 2251; 2267;
   2269; 2273; 2281; 2287; 2293; 2297; 2309; 2311;
   2333; 2339; 2341; 2347; 2351; 2357; 2371; 2377;
   2381; 2383; 2389; 2393; 2399; 2411; 2417; 2423;
   2437; 2441; 2447; 2459; 2467; 2473; 2477; 2503;
   2521; 2531; 2539; 2543; 2549; 2551; 2557; 2579;
   2591; 2593; 2609; 2617; 2621; 2633; 2647; 2657;
   2659; 2663; 2671; 2677; 2683; 2687; 2689; 2693;
   2699; 2707; 2711; 2713; 2719; 2729; 2731; 2741;
   2749; 2753; 2767; 2777; 2789; 2791; 2797; 2801;
   2803; 2819; 2833; 2837; 2843; 2851; 2857; 2861;
   2879; 2887; 2897; 2903; 2909; 2917; 2927; 2939;
   2953; 2957; 2963; 2969; 2971; 2999; 3001; 3011;
   3019; 3023; 3037; 3041; 3049; 3061; 3067; 3079;
   3083; 3089; 3109; 3119; 3121; 3137; 3163; 3167;
   3169; 3181; 3187; 3191; 3203; 3209; 3217; 3221;
   3229; 3251; 3253; 3257; 3259; 3271; 3299; 3301;
   3307; 3313; 3319; 3323; 3329; 3331; 3343; 3347;
   3359; 3361; 3371; 3373; 3389; 3391; 3407; 3413;
   3433; 3449; 3457; 3461; 3463; 3467; 3469; 3491;
   3499; 3511; 3517; 3527; 3529; 3533; 3539; 3541;
   3547; 3557; 3559; 3571; 3581; 3583; 3593; 3607;
   3613; 3617; 3623; 3631; 3637; 3643; 3659; 3671;
   3673; 3677; 3691; 3697; 3701; 3709; 3719; 3727;
   3733; 3739; 3761; 3767; 3769; 3779; 3793; 3797;
   3803; 3821; 3823; 3833; 3847; 3851; 3853; 3863;
   3877; 3881; 3889; 3907; 3911; 3917; 3919; 3923;
   3929; 3931; 3943; 3947; 3967; 3989; 4001; 4003;
   4007; 4013; 4019; 4021; 4027; 4049; 4051; 4057;
   4073; 4079; 4091; 4093; 4099; 4111; 4127; 4129;
   4133; 4139; 4153; 4157; 4159; 4177; 4201; 4211;
   4217; 4219; 4229; 4231; 4241; 4243; 4253; 4259;
   4261; 4271; 4273; 4283; 4289; 4297; 4327; 4337;
   4339; 4349; 4357; 4363; 4373; 4391; 4397; 4409;
   4421; 4423; 4441; 4447; 4451; 4457; 4463; 4481;
   4483; 4493; 4507; 4513; 4517; 4519; 4523; 4547;
   4549; 4561; 4567; 4583; 4591; 4597; 4603; 4621;
   4637; 4639; 4643; 4649; 4651; 4657; 4663; 4673;
   4679; 4691; 4703; 4721; 4723; 4729; 4733; 4751;
   4759; 4783; 4787; 4789; 4793; 4799; 4801; 4813;
   4817; 4831; 4861; 4871; 4877; 4889; 4903; 4909;
   4919; 4931; 4933; 4937; 4943; 4951; 4957; 4967;
   4969; 4973; 4987; 4993; 4999; 5003; 5009; 5011;
   5021; 5023; 5039; 5051; 5059; 5077; 5081; 5087;
   5099; 5101; 5107; 5113; 5119; 5147; 5153; 5167;
   5171; 5179; 5189; 5197; 5209; 5227; 5231; 5233;
   5237; 5261; 5273; 5279; 5281; 5297; 5303; 5309;
   5323; 5333; 5347; 5351; 5381; 5387; 5393; 5399;
   5407; 5413; 5417; 5419; 5431; 5437; 5441; 5443;
   5449; 5471; 5477; 5479; 5483; 5501; 5503; 5507;
   5519; 5521; 5527; 5531; 5557; 5563; 5569; 5573;
   5581; 5591; 5623; 5639; 5641; 5647; 5651; 5653;
   5657; 5659; 5669; 5683; 5689; 5693; 5701; 5711;
   5717; 5737; 5741; 5743; 5749; 5779; 5783; 5791;
   5801; 5807; 5813; 5821; 5827; 5839; 5843; 5849;
   5851; 5857; 5861; 5867; 5869; 5879; 5881; 5897;
   5903; 5923; 5927; 5939; 5953; 5981; 5987; 6007;
   6011; 6029; 6037; 6043; 6047; 6053; 6067; 6073;
   6079; 6089; 6091; 6101; 6113; 6121; 6131; 6133;
   6143; 6151; 6163; 6173; 6197; 6199; 6203; 6211;
   6217; 6221; 6229; 6247; 6257; 6263; 6269; 6271;
   6277; 6287; 6299; 6301; 6311; 6317; 6323; 6329;
   6337; 6343; 6353; 6359; 6361; 6367; 6373; 6379;
   6389; 6397; 6421; 6427; 6449; 6451; 6469; 6473;
   6481; 6491; 6521; 6529; 6547; 6551; 6553; 6563;
   6569; 6571; 6577; 6581; 6599; 6607; 6619; 6637;
   6653; 6659; 6661; 6673; 6679; 6689; 6691; 6701;
   6703; 6709; 6719; 6733; 6737; 6761; 6763; 6779;
   6781; 6791; 6793; 6803; 6823; 6827; 6829; 6833;
   6841; 6857; 6863; 6869; 6871; 6883; 6899; 6907;
   6911; 6917; 6947; 6949; 6959; 6961; 6967; 6971;
   6977; 6983; 6991; 6997; 7001; 7013; 7019; 7027;
   7039; 7043; 7057; 7069; 7079; 7103; 7109; 7121;
   7127; 7129; 7151; 7159; 7177; 7187; 7193; 7207;
   7211; 7213; 7219; 7229; 7237; 7243; 7247; 7253;
   7283; 7297; 7307; 7309; 7321; 7331; 7333; 7349;
   7351; 7369; 7393; 7411; 7417; 7433; 7451; 7457;
   7459; 7477; 7481; 7487; 7489; 7499; 7507; 7517;
   7523; 7529; 7537; 7541; 7547; 7549; 7559; 7561;
   7573; 7577; 7583; 7589; 7591; 7603; 7607; 7621;
   7639; 7643; 7649; 7669; 7673; 7681; 7687; 7691;
   7699; 7703; 7717; 7723; 7727; 7741; 7753; 7757;
   7759; 7789; 7793; 7817; 7823; 7829; 7841; 7853;
   7867; 7873; 7877; 7879; 7883; 7901; 7907; 7919;
   7927; 7933; 7937; 7949; 7951; 7963; 7993; 8009;
   8011; 8017; 8039; 8053; 8059; 8069; 8081; 8087;
   8089; 8093; 8101; 8111; 8117; 8123; 8147; 8161;
   8167; 8171; 8179; 8191]

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
