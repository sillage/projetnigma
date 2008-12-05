let initrandom (n) =
  Random.self_init ();
  Random.int (n-1)

let legendre n =
  let a = initrandom n in
    float_of_int(a)/.float_of_int(n)

let estpreumz n =
  let a = legendre n *. (float_of_int n) in
    if ((legendre n) < 10.**(-5.)) && (int_of_float((a)**float_of_int(n-1) /. 2.) mod n <> int_of_float(legendre n)) then
      false
    else
      true

let probapreumz n k =
  let compt = ref 0 in
  let total = ref 0 in
    for i = 1 to k do
      if estpreumz n then (compt := !compt + 1);
      total := !total + 1
    done;
    float_of_int(!compt) /. float_of_int(!total);;

legendre 22 ;;

