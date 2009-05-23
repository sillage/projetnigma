let addRoundKey () =
  true

let invMixColumns () =
  true

let invShiftRows () =
  true

let invSubBytes () =
  true

let k = 1

let mixColumns state =
  true

let nb = 4

let nk = 4

let nr = 10

let rcon = [||]

let rotWord () =
  true

let matrix_mul a b =
  let aux = Array.make_matrix 4 nb 0 in
    for r = 0 to 3 do
      for c = 0 to (nb - 1) do
	for k = 0 to (nb - 1) do
	  aux.(c).(r) <- aux.(c).(r) + a.(c).(k) * b.(k).(c)
	done;
      done;
    done;
    aux

let shiftRows state =
  let aux = Array.make_matrix 4 nb 0 in
    for r = 0 to 3 do
      for c = 0 to (nb - 1) do
	aux.(r).(c) <- state.(r).((c + r) mod nb);
      done;
    done;
    aux

let subBytes state =
  true

let subWord () =
  true
