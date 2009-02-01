let rec solovay n =
  Random.self_init ();
  let a = ref (Random.int (n-2)) in
  let x = (!a+1)/n in
    if (x = 0) || (int_of_float(float_of_int(!a)**float_of_int(((n-1)/2))) mod n <> x) then
      false
    else
      true

let testprem n k =
  let proba = ref 0. in
  let vrai = ref 0. in
  let faux = ref 0. in
    while k > 0 do
      begin
	if solovay n then
	  vrai := !vrai +. 1.
	else
	  faux := !faux +. 1.
      end
    done;
    proba := !vrai /. (!vrai +. !faux);!proba

let poney = testprem 12 2
let main () =
  print_float(testprem (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2))))

let _ = main ()
