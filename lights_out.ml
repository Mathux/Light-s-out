open Graphics
       
let graphics_open a b =
  open_graph (" " ^ string_of_int (a) ^ ("x") ^ (string_of_int (b)));
  set_window_title "Light's out"
  (*resize_window a b*)

let enCours = ref false
let toggle_sound = ref true
                       
let play_sound () =
  let aux_sound () = 
    if !enCours then ()
    else begin
        enCours := true;
        let _ = Unix.open_process_in "./sound " in
        Unix.sleepf 0.2;
        enCours := false
      end in
  if !toggle_sound then let _ = Thread.create aux_sound () in ()
  else ()
                                      
let sommeZ a b d = (d + a + b) mod d

let egalZ a b d = (a - b + d) mod d = 0

let prodZ a b d = match a, b with
  | (0, _) | (_, 0) -> 0
  | _ -> (d + a * b) mod d

let creerMatZ n p = Array.make_matrix n p 0

let idZ n =
  let m = creerMatZ n n in
  for i = 0 to n - 1 do
	m.(i).(i) <- 1
  done;
  m

let afficheU v n p =
  print_newline ();
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  print_string " | "; print_int v.(i).(j);
	done;
	print_string " | ";
	print_newline ();
  done;
  print_newline ()


let afficheD v n p =
  print_newline ();
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  print_string " | ";
	  if v.(i).(j) < 10 then print_string " ";
	  print_int v.(i).(j);
	done;
	print_string " | ";
	print_newline ();
  done;
  print_newline ()


let afficheC v n p =
  print_newline ();
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  print_string " | ";
	  if v.(i).(j) < 10 then print_string "  "
	  else if v.(i).(j) < 100 then print_string " ";
	  print_int v.(i).(j);
	done;
	print_string " | ";
	print_newline ();
  done;
  print_newline ()

let max_of_mat a n p =
  let m = ref a.(0).(0) in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  if a.(i).(j) > !m then m := a.(i).(j)
	done
  done;
  !m


let affiche a =
  let n = Array.length a and p = Array.length a.(0) in
  let m = max_of_mat a n p in
  match (m / 10) > 0, (m / 100) > 0 with
  | _, true -> afficheC a n p
  | true, _ -> afficheD a n p
  | _ -> afficheU a n p



let inverse n d =
  let c = ref 0 in
  for i = d - 1 downto 0 do
	if (prodZ n i d) = 1 then
	  c := i
  done;
  !c

let divT d =
  let div = (Array.make d 0) in
  for i = 0 to d - 1 do
	div.(i) <- (inverse i d)
  done;
  div

exception Matrice_non_inversible

let triZ n =
  let m = idZ n in
  for i = 1 to n - 2 do
	m.(i).(i + 1) <- 1;
	m.(i).(i - 1) <- 1;
	m.(i + 1).(i) <- 1;
	m.(i - 1).(i) <- 1;
  done;
  m.(0).(1) <- 1;
  m.(1).(0) <- 1;
  m

let abs a = match a >= 0 with
  | true -> a
  | _ -> - a


let matPZ p n =
  let m = creerMatZ (n * p) (n * p) and gT = triZ n and gI = idZ n in
  for i = 0 to p - 1 do (*Indice des lignes-blocs*)
	for j = 0 to p - 1 do (*Indice des colonnes-blocs*)
	  if i = j then begin
		  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- gT.(k).(l)
			done
		  done
		end;
	  if i = j + 1 || i = j - 1 then begin
		  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- gI.(k).(l)
			done
		  done
		end
	done
  done;
  m

let matExtrZ m a b =
  let n = Array.length m in
  let p = creerMatZ (n - 1) (n - 1) in
  for i = 0 to a - 1 do
	for j = 0 to b - 1 do
	  p.(i).(j) <- m.(i).(j);
	done;
	for j = b + 1 to n - 1 do
	  p.(i).(j - 1) <- m.(i).(j);
	done
  done;
  for i = a + 1 to n - 1 do
	for j = 0 to b - 1 do
	  p.(i - 1).(j) <- m.(i).(j);
	done;
	for j = b + 1 to n - 1 do
	  p.(i - 1).(j - 1) <- m.(i).(j);
	done
  done;
  p

let pom j = match j mod 2 with
  | 0 -> 1
  | _ -> - 1


let rec detZ m d =
  let s = ref 0 in
  match (Array.length m) with
  | 2 -> sommeZ (prodZ m.(0).(0) m.(1).(1) d) (prodZ m.(1).(0) m.(0).(1) d) d
  | n -> for j = 0 to n - 1 do
		  if m.(0).(j) != 0 then
			s := sommeZ !s ((pom j) * (detZ (matExtrZ m 0 j) d)) d;
		done;
		!s

let rec det m =
  let s = ref 0 in
  match (Array.length m) with
  | 2 -> (m.(0).(0)*m.(1).(1)) + (m.(1).(0)*m.(0).(1))
  | n -> for j = 0 to n - 1 do
		  if m.(0).(j) != 0 then
			s := !s + ((pom j) * (det (matExtrZ m 0 j) ));
		done;
		!s

let transvectionZ m i j y d = (*On ajoute a la ligne i de m y fois la ligne j*)
	let n = Array.length m.(0) in
	for k = 0 to n - 1 do
		m.(i).(k) <- sommeZ m.(i).(k) (prodZ y m.(j).(k) d) d
	done;;

let echange_lignesZ m i j = let l = m.(i) in
	m.(i) <- m.(j);
	m.(j) <- l;;


let indice_pivotZ m i div = let n = Array.length m and im = ref i in
	for j = i + 1 to n - 1 do
		if not (m.(!im).(i) = 1) && (div.(m.(!im).(i)) = 0) && not (m.(j).(i) = 0) then im := j;
	done;
	!im;;

let copie_matrice m = let n = Array.length m and p = Array.length m.(0) in
	let a = creerMatZ n p in
	for i = 0 to n - 1 do
		for j = 0 to p - 1 do
			a.(i).(j) <- m.(i).(j)
		done
	done;
	a;;

let gauss a0 b0 d = let div = divT d and a = copie_matrice a0 and b = copie_matrice b0 and n = Array.length a0 in
	let j = ref 0 and y = ref 0 and s = ref 0 in
	affiche a;
	for i = 0 to n - 1 do
		j := indice_pivotZ a i div;
		if not (!j = i) then begin
				echange_lignesZ a i !j;
				echange_lignesZ b i !j;
				print_string "L"; print_int i; print_string " <-> L"; print_int !j; print_newline ();
				affiche a;
			end;
		for k = i + 1 to n - 1 do
			y := prodZ (- a.(k).(i)) div.(a.(i).(i)) d;
			transvectionZ a k i !y d;
			transvectionZ b k i !y d;
			print_string "L"; print_int k; print_string " <- L"; print_int k; print_string " + "; print_int !y; print_string " * L"; print_int i; print_newline ();
			affiche a;
		done
	done;
	affiche a;
	let x = creerMatZ n 1 in
	for i = n - 1 downto 0 do
		s := 0;
		for k = i + 1 to n - 1 do
			s := sommeZ !s (prodZ a.(i).(k) x.(k).(0) d) d
		done;
		x.(i).(0) <- sommeZ (- !s) (prodZ div.(a.(i).(i)) (b.(i).(0)) d) d;
	done;
	x



let convertJtoL m =
  let n = Array.length m and p = Array.length m.(0) in
  let a = creerMatZ (n * p) 1 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  a.(i * p + j).(0) <- m.(i).(j)
	done
  done;
  a

let convertLtoJ m n p =
  let a = creerMatZ n p in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  a.(i).(j) <- m.(p * i + j).(0)
	done
  done;
  a

let creerfacileJ l n p =
  let a = creerMatZ (n * p) 1 and t = ref l in
  while not ((!t mod 10) = 0) do
	let u = !t mod 10 in
	t := !t / 10;
	a.(u - 1).(0) <- 1;
  done;
  affiche a;
  convertLtoJ a n p

let affiche2 v =
  print_newline ();
  let n = Array.length v and p = Array.length v.(0) in
  for i = 0 to n - 1 do
	for j = 0 to p - 2 do
	  print_int v.(i).(j); print_string ",";
	done;
	print_int v.(i).(p - 1);
	print_newline ();
  done;
  print_newline ()


let inverserMatZ a0 d =
  let div = divT d and a = copie_matrice a0 and n = Array.length a0 in
  let b = idZ (n) in
  let j = ref 0 and y = ref 0 in 
  affiche a;
  for i = 0 to n - 1 do
	j := indice_pivotZ a i div;
	if not (!j = i) then begin
		echange_lignesZ a i !j;
		echange_lignesZ b i !j;
		print_string "L"; print_int i; print_string " <-> L"; print_int !j; print_newline ();
		affiche a;
	  end;
	for k = i + 1 to n - 1 do
	  y := prodZ (- a.(k).(i)) (div.(a.(i).(i))) d;
	  transvectionZ a k i !y d;
	  transvectionZ b k i !y d;
	  print_string "L"; print_int k; print_string " <- L"; print_int k; print_string " + "; print_int !y; print_string " * L"; print_int i; print_newline ();
	  affiche a;
	done
  done;
  for j = n - 1 downto 0 do
	for i = j - 1 downto 0 do
	  y := prodZ (- a.(i).(j)) (div.(a.(j).(j))) d;
	  transvectionZ a i j !y d;
	  transvectionZ b i j !y d;
	  print_string "L"; print_int i; print_string " <- L"; print_int i; print_string " + "; print_int !y; print_string " * L"; print_int j; print_newline ();
	  affiche a;
	done
  done;
  b


let inverserMatZ a0 d =
  let div = divT d and a = copie_matrice a0 and n = Array.length a0 in
  let b = idZ (n) in
  let j = ref 0 and y = ref 0 in
  for i = 0 to n - 1 do
	j := indice_pivotZ a i div;
	if not (!j = i) then begin
		echange_lignesZ a i !j;
		echange_lignesZ b i !j
	  end;
	for k = i + 1 to n - 1 do
	  y := prodZ (- a.(k).(i)) (div.(a.(i).(i))) d;
	  transvectionZ a k i !y d;
	  transvectionZ b k i !y d;
	done
  done;
  for j = n - 1 downto 0 do
	for i = j - 1 downto 0 do
	  y := prodZ (- a.(i).(j)) (div.(a.(j).(j))) d;
	  transvectionZ a i j !y d;
	  transvectionZ b i j !y d
	done
  done;
  b

let prodMatZ a b d =
  let n = Array.length a and p = Array.length b.(0) and u = Array.length a.(0) in
  let m = creerMatZ n p in
  let s = ref 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  s := 0;
	  for k = 0 to u - 1 do
		s := sommeZ !s (prodZ (a.(i).(k)) (b.(k).(j)) d) d
	  done;
	  m.(i).(j) <- !s
	done
  done;
  m

let sommeMatZ a b n p d =
  let s = Array.make_matrix n p 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  s.(i).(j) <- (a.(i).(j) + b.(i).(j)) mod d
	done
  done;
  s

let egalMatZ a b d =
  let n = Array.length a and p = Array.length a.(0) in
  let n2 = Array.length b and p2 = Array.length b.(0) in
  let bo = ref ((n = n2) && (p = p2)) in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  bo := !bo && ((a.(i).(j) mod d) = ((b.(i).(j)) mod d))
	done
  done;
  !bo

let enumInvZ d =
  print_newline (); print_string "Dans Z/"; print_int d; print_string "Z les inverses sont : "; print_newline (); print_newline ();
  for i = 0 to d - 1 do
	print_int i; print_string " --> "; print_int (inverse i d); print_newline ();
  done;
  print_newline ()


let transformJ m i j n p k d =
  m.(i).(j) <- sommeZ m.(i).(j) k d;
  if not (i = 0) then m.(i - 1).(j) <- sommeZ m.(i - 1).(j) k d
  else m.(n - 1).(j) <- sommeZ m.(n - 1).(j) k d;
  
  if not (i = n - 1) then m.(i + 1).(j) <- sommeZ m.(i + 1).(j) k d
  else m.(0).(j) <- sommeZ m.(0).(j) k d;
  
  if not (j = 0) then m.(i).(j - 1) <- sommeZ m.(i).(j - 1) k d
  else m.(i).(p - 1) <- sommeZ m.(i).(p - 1) k d;
  
  if not (j = p - 1) then m.(i).(j + 1) <- sommeZ m.(i).(j + 1) k d
  else m.(i).(0) <- sommeZ m.(i).(0) k d


let transformJ m i j n p k d =
  m.(i).(j) <- sommeZ m.(i).(j) k d;
  if not (i = 0) then m.(i - 1).(j) <- sommeZ m.(i - 1).(j) k d;
  if not (i = n - 1) then m.(i + 1).(j) <- sommeZ m.(i + 1).(j) k d;
  if not (j = 0) then m.(i).(j - 1) <- sommeZ m.(i).(j - 1) k d;
  if not (j = p - 1) then m.(i).(j + 1) <- sommeZ m.(i).(j + 1) k d



let matA n p d =
  let l = creerMatZ n p in
  Random.self_init ();
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  transformJ l i j n p (Random.int d) d
	done
  done;
  l

let algoZ m n p d =
  for i = 1 to n - 1 do
	for j = 0 to p - 1 do
	  let u = (d - m.(i - 1).(j)) mod d in
	  transformJ m i j n p u d;
	done
  done


let algoZV m n p l d =
  for i = 1 to n - 1 do
	for j = 0 to p - 1 do
	  let u = (d - m.(i - 1).(j)) mod d in
	  transformJ m i j n p u d;
	  l.(i).(j) <- u;
	done
  done


let coeffxMatZ k m n p d =
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  m.(i).(j) <- (k * (m.(i).(j))) mod d
	done
  done

let coeffxMatZE k a n p d =
  let m = copie_matrice a in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  m.(i).(j) <- (k * (m.(i).(j))) mod d
	done
  done;
  m

let coeffxVectZ k m n d =
  for i = 0 to n - 1 do
	m.(i) <- (k * (m.(i))) mod d
  done

let block = 50

let textfin n p block =
  set_text_size block;
  (*set_font "MONACO";*)
  set_color black;
  let (w, h) = text_size "Bravo!" in
  moveto ((block * p) / 2 - w / 2) ((block * n) / 2 - h / 2);
  if w <= block * p && h <= block * n then
	draw_string "Bravo!"

let grey = rgb 165 165 165
let orange = rgb 255 165 0
let purple = rgb 165 0 255
let pink = rgb 255 200 255
let lblue = rgb 200 255 255
let skin = rgb 255 222 190
let brown = rgb 50 134 69
let brown = rgb 50 50 50
let dark_green = rgb 0 155 0

let greyflred = rgb 100 75 75

(*testCouleur greyflred
testCouleur grey
 *)
let tabC i d =
  if d <= 15 then
    begin match i with
	| 1 -> grey
	| 3 -> yellow
	| 2 -> red
	| 4 -> green
	| 5 -> blue
	| 6 -> magenta
	| 7 -> cyan
	| 8 -> orange
	| 9 -> purple
	| 10 -> pink
	| 11 -> skin
	| 12 -> brown
	| 13 -> dark_green
	| 14 -> greyflred
	| _ -> white
	end
  else
    begin
	  let y = (16 * 255) / (d) in
	  let r = (i mod 16) in
	  let nb = (16 * i) / d in
	  match r, i with
	  | _, 0 -> rgb 255 255 255
	  | 1, _ -> rgb (255) (nb * y) (nb * y)
	  | 2, _ -> rgb (nb * y) 255 (nb * y)
	  | 3, _ -> rgb (nb * y) (nb * y) 255
	  | 4, _ -> rgb (nb * y) 255 255
	  | 5, _ -> rgb 255 (nb * y) 255
	  | 6, _ -> rgb 255 255 (nb * y)
	  | 7, _ -> rgb (255 - nb * y) 0 255
	  | 8, _ -> rgb (255 - nb * y) 255 0
	  | 9, _ -> rgb 255 (255 - nb * y) 0
	  | 10, _ -> rgb 0 (255 - nb * y) 255
	  | 11, _ -> rgb 255 0 (255 - nb * y)
	  | 12, _ -> rgb 0 255 (255 - nb * y)
	  | 13, _ -> rgb 0 0 (255 - nb * y)
	  | 14, _ -> rgb 0 (255 - nb * y) 0
	  | 15, _ -> rgb (255 - nb * y) 0 0
	  | 0, _ -> rgb (nb * y) (nb * y) (nb * y)
	  | _, _ -> print_string "OHHH"; print_int nb; print_newline (); rgb 0 0 0
	end


let testCouleur color =
  graphics_open (500) (500);
  set_color color;
  fill_rect 0 0 500 500;
  set_color black;
  fill_rect 50 50 25 25;
  let rec wait () =
	let e = wait_next_event [Key_pressed] in
	if e.keypressed then
	  close_graph ()
	else wait ()
  in wait ()


let combiZ n p d =
  let m = creerMatZ n p and u = Array.make_matrix p p 0 in
  for j = 0 to p - 1 do
	transformJ m 0 j n p 1 d;
	algoZ m n p d;
	for k = 0 to p - 1 do
	  if not (m.(n - 1).(k) = 0) then begin
		  u.(k).(j) <- m.(n - 1).(k);
		  m.(n - 1).(k) <- 0;
		end;
	done
  done;
  u


let transpose m n p =
  let u = Array.make_matrix p n 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  u.(j).(i) <- m.(i).(j)
	done
  done;
  u

let matsy m n p =
  let u = Array.make_matrix p 1 0 in
  for i = 0 to p - 1 do
	u.(i).(0) <- m.(n - 1).(i)
  done;
  u

let multiplie_ligne a i k p d =
  for j = 0 to p - 1 do
	a.(i).(j) <- k * a.(i).(j) mod d
  done

let invD a0 d =
  let div = divT d and a = copie_matrice a0 and n = Array.length a0 in
  let b = idZ (n) in
  let j = ref 0 and y = ref 0 in
  for i = 0 to n - 1 do
	j := indice_pivotZ a i div;
	if not (!j = i) then begin
		echange_lignesZ a i !j;
		echange_lignesZ b i !j;
	  end;
	for k = i + 1 to n - 1 do
	  if div.(a.(i).(i)) = 0 && not (a.(i).(i) = 0) && not (a.(k).(i) = 0) then begin
		  let comp = ref 1 in
		  while not (a.(k).(i) = (!comp * a.(i).(i))) && not (!comp = d) do
			incr comp;
		  done;
		  if not (!comp = d) then begin
			  transvectionZ a k i (d - !comp) d;
			  transvectionZ b k i (d - !comp) d;
			end;
		end
	  else begin
		  y := prodZ (d - a.(k).(i)) div.(a.(i).(i)) d;
		  if not (!y = 0) then begin
			  transvectionZ a k i !y d;
			  transvectionZ b k i !y d;
			end;
		end;
	done
  done;
  for j = n - 1 downto 0 do
	for i = j - 1 downto 0 do
	  y := prodZ (- a.(i).(j)) div.(a.(j).(j)) d;
	  if not (!y = 0) then begin
		  transvectionZ a i j !y d;
		  transvectionZ b i j !y d;
		end;
	done
  done;
  for i = 0 to n - 1 do
	let k = div.(a.(i).(i)) in
	if not (k = 0) then begin
		a.(i).(i) <- a.(i).(i) * k mod d;
		multiplie_ligne b i k n d;
	  end;
  done;
  affiche a; affiche b;
  a, b

let invD a0 d =
  let div = divT d and a = copie_matrice a0 and n = Array.length a0 in
  let b = idZ (n) in
  let j = ref 0 in
  for i = 0 to n - 1 do
	j := indice_pivotZ a i div;
	if not (!j = i) then begin
		echange_lignesZ a i !j;
		echange_lignesZ b i !j;
	  end;
	for k = i + 1 to n - 1 do
	  let q1 = a.(i).(i) and q2 = - a.(k).(i) in
	  if not (q1 = 0) then begin
		  multiplie_ligne a k q1 n d;
		  multiplie_ligne b k q1 n d end;
	  transvectionZ a k i q2 d;
	  transvectionZ b k i q2 d;
	done
  done;
  for j = n - 1 downto 0 do
	for i = j - 1 downto 0 do
	  let q1 = a.(j).(j) and q2 = - a.(i).(j) in
	  let y = prodZ q2 div.(q1) d in
	  if not (y = 0) then begin
		  transvectionZ a i j y d;
		  transvectionZ b i j y d end;
	done
  done;
  for i = 0 to n - 1 do
	let k = div.(a.(i).(i)) in
	if not (k = 0) then begin
		multiplie_ligne a i k n d;
		multiplie_ligne b i k n d;
	  end;
  done;
  (*affiche a; affiche b;*)
  a, b

let decompRE n1 p d =
  let div = divT d and a = matPZ n1 p and n = n1 * p in
  let b = idZ (n) in
  let j = ref 0 in
  for i = 0 to n - 1 do
	j := indice_pivotZ a i div;
	if not (!j = i) then begin
		echange_lignesZ a i !j;
		echange_lignesZ b i !j;
	  end;
	for k = i + 1 to n - 1 do
	  let q1 = a.(i).(i) and q2 = - a.(k).(i) in
	  if not (q1 = 0) then begin
		  multiplie_ligne a k q1 n d;
		  multiplie_ligne b k q1 n d end;
	  transvectionZ a k i q2 d;
	  transvectionZ b k i q2 d;
	done
  done;
  for j = n - 1 downto 0 do
	for i = j - 1 downto 0 do
	  let q1 = a.(j).(j) and q2 = - a.(i).(j) in
	  let y = prodZ q2 div.(q1) d in
	  if not (y = 0) then begin
		  transvectionZ a i j y d;
		  transvectionZ b i j y d end;
	done
  done;
  for i = 0 to n - 1 do
	let k = div.(a.(i).(i)) in
	if not (k = 0) then begin
		multiplie_ligne a i k n d;
		multiplie_ligne b i k n d;
	  end;
  done;
  b, a

let decompREA n1 p d aA =
  let div = divT d and n = n1 * p and a = copie_matrice aA in
  let b = idZ (n) in
  let j = ref 0 in
  for i = 0 to n - 1 do
	j := indice_pivotZ a i div;
	if not (!j = i) then begin
		echange_lignesZ a i !j;
		echange_lignesZ b i !j;
	  end;
	for k = i + 1 to n - 1 do
	  let q1 = a.(i).(i) and q2 = - a.(k).(i) in
	  if not (q1 = 0) then begin
		  multiplie_ligne a k q1 n d;
		  multiplie_ligne b k q1 n d end;
	  transvectionZ a k i q2 d;
	  transvectionZ b k i q2 d;
	done
  done;
  for j = n - 1 downto 0 do
	for i = j - 1 downto 0 do
	  let q1 = a.(j).(j) and q2 = - a.(i).(j) in
	  let y = prodZ q2 div.(q1) d in
	  if not (y = 0) then begin
		  transvectionZ a i j y d;
		  transvectionZ b i j y d end;
	done
  done;
  for i = 0 to n - 1 do
	let k = div.(a.(i).(i)) in
	if not (k = 0) then begin
		multiplie_ligne a i k n d;
		multiplie_ligne b i k n d;
	  end;
  done;
  b, a


let equaline a b d =
  let x = ref 0 and j = ref 0 in
  while not (egalZ (a * !x) (b) d && !j < d) do
	incr j;
	x := !j
  done;
  if !j = d && not (b = 0) then begin print_int a; print_string "*x = "; print_int b; print_string " mod "; print_int d; print_string " n'a pas de solution."; print_newline (); (*raise matrice_non_inversible*)
		                     end;
  !x mod d

let gausse a0 b0 d =
  let div = divT d and a = copie_matrice a0 and b = copie_matrice b0 and n = Array.length a0 in
  let j = ref 0 and  s = ref 0 in
  for i = 0 to n - 1 do
	j := indice_pivotZ a i div;
	if not (!j = i) then begin
		echange_lignesZ a i !j;
		echange_lignesZ b i !j;
	  end;
	for k = i + 1 to n - 1 do
	  let q1 = a.(i).(i) and q2 = - a.(k).(i) in
	  if not (q1 = 0) then begin
		  multiplie_ligne a k q1 n d;
		  b.(k).(0) <- (q1 * b.(k).(0)) mod d
		end;
	  transvectionZ a k i q2 d;
	  transvectionZ b k i q2 d;
	done
  done;
  let x = creerMatZ n 1 in
  for i = n - 1 downto 0 do
	s := 0;
	for k = i + 1 to n - 1 do
	  s := sommeZ !s (prodZ a.(i).(k) x.(k).(0) d) d
	done;
	x.(i).(0) <- equaline a.(i).(i) ((b.(i).(0) - !s + d) mod d) d;
  done;
  (*affiche a;*) x


let sol m n p d =
  let u = combiZ n p d and jJ = (matsy m n p) in
  coeffxMatZ (d - 1) jJ p 1 d;
  let x = gauss u jJ d in
  transpose x p 1

let sol2 m n p d =
  let u = combiZ n p d and jJ = (matsy m n p) in
  coeffxMatZ (d - 1) jJ p 1 d;
  let x = gausse u jJ d in
  (transpose x p 1)

let executeTest a m n p d =
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  transformJ m i j n p (a.(i).(j)) d
	done
  done;
  let u = m = (Array.make_matrix n p 0) in
  if u then begin print_string "La solution marche" end
  else print_string "La solution ne marche pas";
  print_newline ();
  u



let rang m n d =
  let a = fst (invD m d) and r = ref 0 in
  let i = ref 0 in
  while !r = 0 && not (!i = n) do
	if a.(!i).(!i) = 0 then incr r;
	incr i;
  done;
  !i - !r

let dimKer m n d =
  let a = fst (invD m d) and r = ref 0 in
  let i = ref 0 in
  while !r = 0 && not (!i = n) do
	if a.(!i).(!i) = 0 then incr r;
	incr i;
  done;
  n - !i + !r, a


let affiche_rang () =
  let tabl = Array.make 12 (Array.make_matrix 11 11 0) in
  for d = 2 to 11 do
	tabl.(d) <- Array.make_matrix 11 11 0;
	print_newline (); print_string "		Dans Z/"; print_int d; print_string "Z :"; print_newline ();
	for i = 2 to 10 do
	  print_newline ();
	  print_string "	Matrice "; print_int i; print_string "x _"; print_newline ();
	  for j = 2 to 10 do
		let r = rang (matPZ i j) (i * j) d in
		print_string "Rang pour "; print_int i; print_string "x"; print_int j; print_string " : "; print_int r; print_string " sur "; print_int (i * j);
		if r = (i * j) then begin
			print_string " inversibilite OUI.";
			(tabl.(d)).(i).(j) <- 1
		  end
		else begin
			print_string " inversibilite NON";
			(tabl.(d)).(i).(j) <- 0
		  end;
		print_newline ();
	  done;
	done
  done;
  tabl

let affiche_dimKer () =
  let tabl = Array.make 12 (Array.make_matrix 11 11 0) in
  for d = 2 to 11 do
	tabl.(d) <- Array.make_matrix 11 11 0;
	print_newline (); print_string "		Dans Z/"; print_int d; print_string "Z :"; print_newline ();
	for i = 2 to 10 do
	  print_newline ();
	  print_string "	Matrice "; print_int i; print_string "x _"; print_newline ();
	  for j = 2 to 10 do
		let k = fst (dimKer (matPZ i j) (i * j) d) in
		print_string "Dim du noyau pour "; print_int i; print_string "x"; print_int j; print_string " : "; print_int k; print_string " sur "; print_int (i * j);
		if k = 0 then begin
			print_string " inversibilite OUI.";
			(tabl.(d)).(i).(j) <- 1
		  end
		else begin
			print_string " inversibilite NON";
			(tabl.(d)).(i).(j) <- 0
		  end;
		print_newline ();
	  done;
	done
  done;
  tabl



let prodscalaire u v n d =
  let r = ref 0 in
  for k = 0 to n - 1 do
	r := (!r + u.(k) * v.(k)) mod d
  done;
  !r

let vect_of_matrix u n =
  let m = Array.make n 0 in
  for i = 0 to n - 1 do
	m.(i) <- u.(i).(0)
  done;
  m

let matrixJ_of_vect u n p =
  let m = Array.make_matrix n p 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  m.(i).(j) <- u.(i * p + j)
	done
  done;
  m

let descentePoursolvable m n p d =
  let a = Array.make_matrix n p 0 in
  let l = Array.make (n * p) 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  transformJ a i j n p (m.(i * p + j)) d
	done
  done;
  for i = 1 to n - 1 do
	for j = 0 to p - 1 do
	  let u = (d - a.(i - 1).(j)) mod d in
	  transformJ a i j n p u d;
	  l.(i * p + j) <- u
	done
  done;
  l


let somme_of_vect a b n d =
  for i = 0 to n - 1 do
	a.(i) <- (a.(i) + b.(i)) mod d
  done


let solvable m n p d =
  let dim, a = (dimKer (matPZ n p) (n * p) d) in
  if dim = 0 then true
  else begin
	  let baseK = Array.make_matrix dim (n * p) 0 in
	  for i = dim - 1 downto 0 do
		for j = 0 to (n * p) - 1 do
		  baseK.(i).(j) <- a.(j).(n * p - 1 - i)
		done
	  done;
	  for i = 0 to dim - 1 do
		somme_of_vect (baseK.(i)) (descentePoursolvable (baseK.(i)) n p d) (n * p) d;
	  done;
	  let u = vect_of_matrix (convertJtoL m) (n * p) in
	  let b = ref true in
	  for i = 0 to dim - 1 do
		affiche (matrixJ_of_vect (baseK.(i)) n p)
	  done;
	  for i = 0 to dim - 1 do
		b := !b && (prodscalaire baseK.(i) u (n * p) d) = 0
	  done;
	  !b;
	end

let quietPatern n p d =
  let dim, a = (dimKer (matPZ n p) (n * p) d) in
  if dim = 0 then Array.make_matrix 1 (n * p) 0
  else begin
	  let baseK = Array.make_matrix dim (n * p) 0 in
	  for i = dim - 1 downto 0 do
		for j = 0 to (n * p) - 1 do
		  baseK.(i).(j) <- a.(j).(n * p - 1 - i)
		done
	  done;
	  for i = 0 to dim - 1 do
		somme_of_vect (baseK.(i)) (descentePoursolvable (baseK.(i)) n p d) (n * p) d;
	  done;
	  baseK
	end



let total m n p =
  let t = ref 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  if not (m.(i).(j) = 0) then incr t
	done
  done;
  !t

let total2 m n p =
  let t = ref 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  t := !t + m.(i).(j)
	done
  done;
  !t

let base10to2 n b =
  let l = Array.make b 0 in
  let i = ref 0 and r = ref n in
  while not (!r / 2 = 0) do
	l.(b - 1 - !i) <- (!r mod 2);
	r := !r / 2;
	incr i
  done;
  l.(b - 1 - !i) <- (!r mod 2);
  l

let base10tod n d b =
  let l = Array.make b 0 in
  let i = ref 0 and r = ref n in
  while not (!r / d = 0) do
	l.(b - 1 - !i) <- (!r mod d);
	r := !r / d;
	incr i
  done;
  l.(b - 1 - !i) <- (!r mod d);
  l


let rec puissance x n = match n with
  | 1 -> x
  | _ -> x * (puissance x (n - 1))



let solution_optimale m n p d =
  let baseK = (quietPatern n p d) in
  let b = Array.length (baseK) in
  let tT = (puissance d b) and nbcoutmin = ref (total m n p) and nbcoutmin2 = ref (total2 m n p) in
  let solution = ref (copie_matrice m) and solution2 = ref (copie_matrice m) in
  for i = 0 to tT - 1 do
	let ne = ref (copie_matrice m) in
	let l = base10tod i d b in
	for k = 0 to b - 1 do
	  if not (l.(k) = 0) then ne := sommeMatZ (!ne) (coeffxMatZE l.(k) (matrixJ_of_vect (baseK.(k)) n p) n p d) n p d;
	done;
	affiche !ne;
	let u = total (!ne) n p in
	let u2 = total2 (!ne) n p in
	if u < !nbcoutmin then begin
		solution := !ne;
		nbcoutmin := u end;
	
	if u2 < !nbcoutmin2 then begin
		solution2 := !ne;
		nbcoutmin2 := u2 end;
  done;
  !solution, !solution2, !nbcoutmin, !nbcoutmin2



let solution_optimale2 m n p d =
  let baseK = (quietPatern n p d) in
  let b = Array.length (baseK) in
  let tT = (puissance d b) and nbcoutmin = ref (total2 m n p) in
  let solution = ref (copie_matrice m) in
  for i = 0 to tT - 1 do
	let ne = ref (copie_matrice m) in
	let l = base10to2 i b in
	for k = 0 to b - 1 do
	  if not (l.(k) = 0) then ne := sommeMatZ (!ne) (coeffxMatZE l.(k) (matrixJ_of_vect (baseK.(k)) n p) n p d) n p d;
	done;
	let u = total2 (!ne) n p in
	if u < !nbcoutmin then solution := !ne
  done;
  !solution


let premiereLigneEnMat y =
  let p = Array.length y.(0) in 
  let n = Array.make_matrix p 1 0 in
  for i=0 to p-1 do
	n.(i).(0)<- y.(0).(i)
  done; n


let trace_jeu m d block =
  let n = Array.length m and p = Array.length m.(0) and ne = ref false in
  let g = copie_matrice m in
  let fin = creerMatZ n p in
  let contour = ref true in
  let soluce = ref (Array.make_matrix n p 0) in
  let indV1 = ref 0 and indV2 = ref 0 in
  graphics_open (block * p) (block * n);
  let pos = ref (0, (n - 1) * block) in
  set_color (tabC 0 d);
  fill_rect 0 0 (block * p) (block * n);
  let case a b = ((- a / (block) + n - 1), (b / block)) in
  let colorJeu b =
	set_color (tabC 0 d);
	fill_rect 0 0 (block * p) (block * n);
	set_color (tabC 0 d);
	for i = 0 to n - 1 do
	  for j = 0 to p - 1 do
		if not (g.(i).(j) = 0) then begin
			set_color (tabC g.(i).(j) d);
			fill_rect (fst !pos) (snd !pos) block block;
		  end;
		pos := ((fst !pos) + block, snd !pos);
	  done;
	  pos := (0, (snd !pos) - block);
	done;
	pos := (0, (n - 1) * block);
	(* Affichage des contours *)
	if b then begin
		set_color black;
		for i = 0 to n - 1 do
		  moveto 0 (i * block);
		  lineto (block * p) (i * block);
		  moveto 0 ((i + 1) * block - 1);
		  lineto (block * p) ((i + 1) * block - 1);
		done;
		for j = 0 to p - 1 do
		  moveto (j * block) 0;
		  lineto (j * block) (block * n);
		  moveto ((j + 1) * block - 1) 0;
		  lineto ((j + 1) * block - 1) (block * n);
		done;
	  end;
  in
  colorJeu !contour;
  
  let colorCase i j b =
	set_color (tabC g.(i).(j) d);
	if b then fill_rect (j * block + 1) ((n - i - 1) * block + 1) (block - 3) (block - 3)
	else fill_rect (j * block) ((n - i - 1) * block) block block
  in
  
  let colorCaseP i j b =
	colorCase i j b;
	if not (i = 0) then colorCase (i - 1) j b;
	if not (i = n - 1) then colorCase (i + 1) j b;
	if not (j = 0) then colorCase i (j - 1) b;
	if not (j = p - 1) then colorCase i (j + 1) b
  in
  
  (*affiche (convertLtoJ (prodMatZ (inverserMatZ (matPZ n p)) (convertJtoL g)) n p);*)
  let rec jeu () =
	let e = wait_next_event [Key_pressed; Button_down] in
	let xX = e.mouse_x and yY = e.mouse_y in
	if e.button then begin
		let u = case yY xX in
		play_sound ();
		transformJ g (fst u) (snd u) n p 1 d;
		if g = fin then begin
			colorJeu false;
			textfin n p block
		  end
		else colorJeu !contour;
		jeu ();
	  end;
	if e.keypressed then begin
		let u = e.key in
		match u with
		| '\027' -> close_graph (); (* Touche echap *)
				   ne := false;
		| '\013' -> begin (* Touche entrer *)
			play_sound ();
			transformJ g (!indV1) (!indV2) n p (!soluce.(!indV1).(!indV2)) d;
			if !indV2 < p - 1 then incr indV2
									    
			else begin
				if !indV1 < n - 1 then begin
					incr indV1;
					indV2 := 0
				  end
			  end;
			if g = fin then begin
				colorJeu false;
				textfin n p block
			  end
			else colorJeu !contour;
			jeu ();
		  end;
		| 's' -> begin
			algoZ g n p d;
			if g = fin then begin
				colorJeu false;
				textfin n p block
			  end
			else colorJeu !contour;
			jeu ();
		  end;
        | 'l' -> toggle_sound := not (!toggle_sound); jeu ()
				
		| 'w' -> begin
			let u = case yY xX in
			play_sound ();
			transformJ g (fst u) (snd u) n p 1 d;
			colorCaseP (fst u) (snd u) !contour;
			jeu ();
		  end;
				
		| 'm' -> begin
			if (executeTest !soluce g n p d) then ();
			colorJeu !contour;
			jeu ();
		  end
		| 'a' -> begin
			let u = case yY xX in
			play_sound ();
			transformJ g (fst u) (snd u) n p 1 d;
			if g = fin then begin
				colorJeu false;
				textfin n p block
			  end
			else colorJeu !contour;
			jeu ();
		  end;
		| 'e' -> begin
			let texec = Sys.time () in
			let l = Array.make_matrix n p 0 and l2 = Array.make_matrix n p 0 in
			let gc = copie_matrice g in
			algoZV gc n p l d;
			let haut = sol2 gc n p d in
			for n = 0 to p - 1 do
			  l.(0).(n) <- haut.(0).(n)
			done;
			for k = 0 to p - 1 do
			  transformJ gc 0 k n p (l.(0).(k)) d;
			done;
			algoZV gc n p l2 d;
			print_string "Obtenue en : "; print_float (1000. *. (Sys.time () -. texec)); print_string " ms"; print_newline ();
			(*print_string "Voici une solution :"; print_newline ();*)
			soluce := sommeMatZ l l2 n p d;
			affiche !soluce;
			
			let gaf = copie_matrice g in
			let test = executeTest !soluce gaf n p d in
			
			print_newline ();
			if test then begin
				print_string "Voulez-vous chercher toutes les solutions? (Y/N)"; print_newline ();
				let qz = wait_next_event [Key_pressed] in
				if qz.keypressed then begin
					let po = qz.key in
					match po with | 'y' -> begin
									  let ker = fst (dimKer (matPZ n p) (n * p) d) in
									  if ker = 0 then begin print_string "Cette solution est unique !"; print_newline () end
									  else begin
										  print_string "Dimension du noyau : "; print_int ker; print_newline ();
										  print_string "Il existe donc exactement "; print_int d; print_string "^"; print_int ker; print_string " = "; print_int (puissance d ker); print_string " solutions possibles."; print_newline ();
										  print_string "Voulez-vous les afficher ? (Y/N)"; print_newline ();
										  let s = wait_next_event [Key_pressed] in
										  if s.keypressed then begin
											  let u = s.key in
											  match u with | 'y' -> begin
															   let soluce1, soluce2, n1, n2 = (solution_optimale (!soluce) n p d) in
															   
															   print_string "Solution optimale ("; print_int n1; print_string " cases  appuyer au total) :"; print_newline ();
															   affiche soluce1;
															   print_string "Solution optimale ("; print_int n2; print_string " appuis au total) :"; print_newline ();
															   affiche soluce2;
															   
															 end
														   | _ -> ()
																   
											end;
										end;
									end
								  | _ -> ()
										  
				  end;
				
				
				
			  end;
			colorJeu !contour;
			
			(* PROBLEME ICI AVEC TEST*)
			
			if not test then begin
				let ind = ref (p - 1) in
				while !ind >= 0 && !soluce.(0).(!ind) = 0 do
				  decr ind
				done;
				incr ind;
				
				(* Inserer ici code pour trouver la solution *)
				
				let nulle = creerMatZ n p and solT = Array.make_matrix p p 0 in
				for j = !ind to p - 1 do
				  transformJ nulle 0 j n p 1 d;
				  algoZ nulle n p d;
				  for k = 0 to p - 1 do
					if not (nulle.(n - 1).(k) = 0) then begin
						solT.(k).(j) <- nulle.(n - 1).(k);
						nulle.(n - 1).(k) <- 0;
						
					  end;
				  done
				done;
				
				let jJ = matsy gc n p in
				coeffxMatZ (d - 1) jJ p 1 d;
				let solup = transpose (gausse solT jJ d) p 1 in
				affiche solup;
				
				let l1 = Array.make_matrix n p 0 and l2 = Array.make_matrix n p 0 in
				for y = 0 to p - 1 do
				  l1.(0).(y) <- solup.(0).(y)
				done;
				
				for k = 0 to p - 1 do
				  transformJ gc 0 k n p (solup.(0).(k)) d;
				done;
				
				algoZV gc n p l2 d;
				soluce := sommeMatZ (sommeMatZ (l1) (l2) n p d) (gaf) n p d;
				print_string "Marche pas";
			  end;
			indV1 := 0;
			indV2 := 0;
			jeu ();
		  end
				  
		| 'x' -> begin
			let nulle = creerMatZ n p and solT = Array.make_matrix p p 0 in
			for j = p - 3 to p - 1 do
			  transformJ nulle 0 j n p 1 d;
			  algoZ nulle n p d;
			  for k = 0 to p - 1 do
				if not (nulle.(n - 1).(k) = 0) then begin
					solT.(k).(j) <- nulle.(n - 1).(k);
					nulle.(n - 1).(k) <- 0;
					print_int 4;
				  end;
			  done
			done;
			
			let jJ = matsy g n p in
			coeffxMatZ (d - 1) jJ p 1 d;
			let solup = transpose (gausse solT jJ d) p 1 in
			affiche solup;
			jeu ();
		  end
		| 'z' -> begin
			let haut = sol2 g n p d in
			for k = 0 to p - 1 do
			  transformJ g 0 k n p (haut.(0).(k)) d;
			done;
			if g = fin then begin
				colorJeu false;
				textfin n p block
			  end
			else colorJeu !contour;
			jeu ();
		  end;
		| 'r' -> begin
			for i = 0 to n - 1 do
			  for j = 0 to p - 1 do
				g.(i).(j) <- m.(i).(j)
			  done
			done;
			if g = fin then begin
				colorJeu false;
				textfin n p block
			  end
			else colorJeu !contour;
			jeu ();
		  end;
		| 'p' -> begin
			contour := not (!contour);
			if g = fin then begin
				colorJeu false;
				textfin n p block
			  end
			else colorJeu !contour;
			jeu ();
		  end
		| 'f' -> begin
			for i = 0 to n - 1 do
			  for j = 0 to p - 1 do
				g.(i).(j) <- 0
			  done
			done;
			colorJeu false;
			textfin n p block;
			jeu ();
		  end
		| 'n' -> begin
			close_graph ();
			ne := true;
		  end
		| 'b' -> begin
			let je = convertJtoL g in
			(coeffxMatZ (d - 1) je (n * p) 1 d); (*
	  				soluce := convertLtoJ (prodMatZ (snd(invD (matPZ n p) d)) je d) n p;*)
			soluce := convertLtoJ (gausse (matPZ n p) je d) n p;
			affiche !soluce;
			indV1 := 0;
			indV2 := 0;
			jeu ();
		  end
		| 'v' -> begin
			let je = convertJtoL g in
			(coeffxMatZ (d - 1) je (n * p) 1 d);
			soluce := convertLtoJ (gausse (matPZ n p) je d) n p;
			affiche !soluce;
			indV1 := 0;
			indV2 := 0;
			jeu ();
		  end
		| 'g' -> begin
			let texec = Sys.time () in
			let sol = (convertLtoJ (coeffxMatZE (d - 1) (prodMatZ (fst (decompRE n p d)) (convertJtoL g) d) (n * p) 1 d) n p) in
			print_string "Obtenue en : "; print_float (1000. *. (Sys.time () -. texec)); print_string " ms"; print_newline ();
			affiche sol;
			let _ = executeTest sol (copie_matrice g) n p d in
			jeu ();
		  end
		| 'j' -> begin
			let texec = Sys.time () in
			let lpetit = premiereLigneEnMat g in
			affiche g;
			affiche lpetit;
			affiche (fst (decompREA 1 p d (triZ p)));
			let sol = (convertLtoJ (coeffxMatZE (d - 1) (prodMatZ (fst (decompREA 1 p d (triZ p))) lpetit d) (p) 1 d) 1 p) in
			print_float (Sys.time () -. texec);
			print_newline ();
			affiche sol;
			jeu ();
		  end
		| 'k' -> begin
			let rec edit () =
			  let s = wait_next_event [Key_pressed; Button_down] in
			  let xX = s.mouse_x and yY = s.mouse_y in
			  if s.button then begin
				  let u = case yY xX in
				  play_sound ();
				  g.(fst u).(snd u) <- (g.(fst u).(snd u) + 1) mod d;
				  colorJeu !contour;
				  edit ()
				end;
			  if s.keypressed then begin
				  let u = s.key in
				  match u with
				  | 'a' -> begin
					  let u = case yY xX in
					  play_sound ();
					  g.(fst u).(snd u) <- (g.(fst u).(snd u) + 1) mod d;
					  colorJeu !contour;
					  edit ()
					end;
				  | '\013' -> ();
				  | _ -> edit ();
				end;
			in edit ();
			   jeu ()
		  end
				  
		| _ -> jeu ()
	  end;
  in jeu ();
	 !ne
      



let newgame n p d block =
  let ne = ref true in
  while !ne do
	ne := trace_jeu (matA n p d) d block
  done


let matCouleur n p d =
  let m = Array.make_matrix n p 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  m.(i).(j) <- (i * p + j) mod d
	done
  done;
  m

let create_matPZA n p d =
  let m = Array.make_matrix (n * p) (n * p) 0 in
  for i = 0 to (n * p) - 1 do
	for j = 0 to (n * p) - 1 do
	  m.(i).(j) <- (Random.int d)
	done
  done;
  m


let transformJA m i j n p k d aA =
  let xX = Array.make_matrix (n * p) 1 0 in
  xX.(p * i + j).(0) <- k;
  let yY = convertLtoJ (prodMatZ aA xX d) n p in
  for w = 0 to n - 1 do
	for v = 0 to p - 1 do
	  m.(w).(v) <- (sommeZ m.(w).(v) yY.(w).(v) d)
	done
  done


let executeTestA v m n p d a =
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  transformJA m i j n p (v.(i).(j)) d a
	done
  done;
  let u = m = (Array.make_matrix n p 0) in
  if u then begin print_string "La solution marche" end
  else print_string "La solution ne marche pas";
  print_newline ();
  u


let trace_jeuA m d a block =
  let n = Array.length m and p = Array.length m.(0) and ne = ref false in
  let g = copie_matrice m in
  let fin = creerMatZ n p in
  let contour = ref true in
  graphics_open (block * p) (block * n);
  let pos = ref (0, (n - 1) * block) in
  set_color (tabC 0 d);
  fill_rect 0 0 (block * p) (block * n);
  let case a b = ((- a / (block) + n - 1), (b / block)) in
  let colorJeu b =
	set_color (tabC 0 d);
	for i = 0 to n - 1 do
	  for j = 0 to p - 1 do
		set_color (tabC g.(i).(j) d);
		fill_rect (fst !pos) (snd !pos) block block;
		pos := ((fst !pos) + block, snd !pos);
	  done;
	  pos := (0, (snd !pos) - block);
	done;
	pos := (0, (n - 1) * block);
	(* Affichage des contours *)
	if b then begin
		set_color black;
		for i = 0 to n - 1 do
		  moveto 0 (i * block);
		  lineto (block * p) (i * block);
		  moveto 0 ((i + 1) * block - 1);
		  lineto (block * p) ((i + 1) * block - 1);
		done;
		for j = 0 to p - 1 do
		  moveto (j * block) 0;
		  lineto (j * block) (block * n);
		  moveto ((j + 1) * block - 1) 0;
		  lineto ((j + 1) * block - 1) (block * n);
		done;
	  end;
  in
  colorJeu !contour;
  let rec jeuX () =
	let e = wait_next_event [Key_pressed; Button_down] in
	let xX = e.mouse_x and yY = e.mouse_y in
	if e.button then begin
		let u = case yY xX in
		play_sound ();
		transformJA g (fst u) (snd u) n p 1 d a;
		if g = fin then begin
			colorJeu false;
			textfin n p block
		  end
		else colorJeu !contour;
		jeuX ();
	  end;
	if e.keypressed then begin
		let u = e.key in
		match u with
		| '\027' -> close_graph (); (* Touche echap *)
				   ne := false;
				   
		| 'a' -> begin
			let u = case yY xX in
			play_sound ();
			transformJA g (fst u) (snd u) n p 1 d a;
			if g = fin then begin
				colorJeu false;
				textfin n p block
			  end
			else colorJeu !contour;
			jeuX ();
		  end;
		| 'g' -> begin
			let sol = (convertLtoJ (coeffxMatZE (d - 1) (prodMatZ (fst (decompREA n p d a)) (convertJtoL g) d) (n * p) 1 d) n p) in
			affiche sol;
			let _ = executeTestA sol (copie_matrice g) n p d a in
			jeuX ();
		  end
		| 'n' -> begin
			close_graph ();
			ne := true;
		  end
		| 'f' -> begin
			for i = 0 to n - 1 do
			  for j = 0 to p - 1 do
				g.(i).(j) <- 0
			  done
			done;
			colorJeu false;
			textfin n p block;
			jeuX ();
		  end
		| _ -> jeuX ()
	  end;
  in jeuX ();
	 !ne

let matAl n p d =
  let m = Array.make_matrix n p 0 in
  for i = 0 to n - 1 do
	for j = 0 to p - 1 do
	  m.(i).(j) <- (Random.int d)
	done
  done;
  m

let newgame2 n p d block =
  let ne = ref true in
  let a = (create_matPZA n p d) in
  while !ne do
	ne := trace_jeuA (convertLtoJ (prodMatZ a (matAl (n * p) 1 d) d) n p) d a block
  done


let triZT n =
  let m = idZ n in
  for i = 1 to n - 2 do
	m.(i).(i + 1) <- 1;
	m.(i).(i - 1) <- 1;
	m.(i + 1).(i) <- 1;
	m.(i - 1).(i) <- 1;
  done;
  m.(0).(1) <- 1;
  m.(1).(0) <- 1;
  m.(n-1).(0) <- 1;
  m.(0).(n-1) <- 1;
  m
	
let matPZT n p =
  let m = creerMatZ (n * p) (n * p) and tT = triZT n and iI = idZ n in
  for i = 0 to p - 1 do (*Indice des lignes-blocs*)
	for j = 0 to p - 1 do (*Indice des colonnes-blocs*)
	  if i = j then begin
		  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- tT.(k).(l)
			done
		  done
		end;
	  if i = j + 1 || i = j - 1 then begin
		  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- iI.(k).(l)
			done
		  done
		end
	done
  done;
  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
	for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
	  m.(k).(l + (p-1) * n) <- iI.(k).(l)
	done
  done;
  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
	for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
	  m.((p-1) * n + k).(l) <- iI.(k).(l)
	done
  done;
  m

let matPZ4 n p =
  let m = creerMatZ (n * p) (n * p) and tT = triZ n and iI = idZ n in
  for i = 0 to p - 1 do (*iIndice des lignes-blocs*)
	for j = 0 to p - 1 do (*iIndice des colonnes-blocs*)
	  if i = j then begin
		  for k = 0 to n - 1 do (*iIndice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*iIndice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- tT.(k).(l);
			  if k=l then m.(i * n + k).(l + j * n) <- 0
			done
		  done
		end;
	  if i = j + 1 || i = j - 1 then begin
		  for k = 0 to n - 1 do (*iIndice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*iIndice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- iI.(k).(l)
			done
		  done
		end
	done
  done;
  m	

let matPZ5 n p =
  let m = creerMatZ (n * p) (n * p) and tT = triZ n in
  for i = 0 to p - 1 do (*Indice des lignes-blocs*)
	for j = 0 to p - 1 do (*Indice des colonnes-blocs*)
	  if i = j then begin
		  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- tT.(k).(l);
			done
		  done
		end;
	  if i = j + 1 || i = j - 1 then begin
		  for k = 0 to n - 1 do (*Indice des lignes dans un bloc*)
			for l = 0 to n - 1 do (*Indice des colonnes dans un bloc*)
			  m.(i * n + k).(l + j * n) <- tT.(k).(l)
			done
		  done
		end
	done
  done;
  m	
	
	
let newgameTORE n p d block =
  let ne = ref true in
  let a = (matPZT n p) in
  while !ne do
	ne := trace_jeuA (convertLtoJ (prodMatZ a (matAl (n * p) 1 d) d) n p) d a block
  done
	

let newgame4 n p d block =
  let ne = ref true in
  let a = (matPZ4 n p) in
  while !ne do
	ne := trace_jeuA (convertLtoJ (prodMatZ a (matAl (n * p) 1 d) d) n p) d a block
  done

let newgame5 n p d block =
  let ne = ref true in
  let a = (matPZ5 n p) in
  while !ne do
	ne := trace_jeuA (convertLtoJ (prodMatZ a (matAl (n * p) 1 d) d) n p) d a block
  done


let explain () =
  Printf.printf "Arguments : n (lignes), m (colonnes) et p (nombres de couleurs)\n"
    
let _ = try
    (match (int_of_string (Sys.argv.(1)),int_of_string (Sys.argv.(2)),int_of_string (Sys.argv.(3))) with
     | (n,m,p) when n>0 && m>0 && p>1 -> begin
         try (newgame n m p (int_of_string (Sys.argv.(4))))
         with | _ -> newgame n m p 100
       end
     | _ -> explain ()
    )
  with
  | _ -> explain ()
                        
    
                (*let _ = newgame 2 2 2 100*)
                (*
newgame5 5 5 2 100

newgame 9 17 11 50*)
