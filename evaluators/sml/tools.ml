let rec apply_alist lst (elt : string) =
  match lst with
    [] -> failwith "apply_alist"
  | (x, value) :: tail ->
      if x = elt
      then value
      else apply_alist tail elt

let can f x = try (f x; true) with _ -> false

let fst (x, y) = x
let snd (x, y) = y

let mapfilter f list =
  List.fold_right
    (fun x lst -> try f x :: lst with _ -> lst)
    list
    []

let decr_steps steps =
  if steps = 0
  then failwith "decr_steps"
  else steps - 1

let split n list =
  let (left, right, n) =
    List.fold_left
      (fun (left,right,n) x ->
	if n <= 0
	then (left, right @ [x], n)
	else (left @ [x], right, n - 1))
      ([], [], n)
      list in
  (left, right)

let rec accumulate2 f i lst1 lst2 =
  match lst1, lst2 with
    [], [] -> i
  | [], _  -> failwith "accumulate2"
  | _, []  -> failwith "accumulate2"
  | (x :: xs), (y :: ys) -> accumulate2 f (f i x y) xs ys

let all2 f lst1 lst2 = try List.for_all2 f lst1 lst2 with _ -> false

let get1 n lst =
  match lst with
    [x1] -> x1
  | _    -> failwith ("get1("
		      ^ string_of_int n
		      ^ "-"
		      ^ string_of_int (List.length lst)
		      ^ ")")

let get2 n lst =
  match lst with
    [x1;x2] -> (x1,x2)
  | _ -> failwith ("get2("
		   ^ string_of_int n
		   ^ "-"
		   ^ string_of_int (List.length lst)
		   ^ ")")

let get3 n lst =
  match lst with
    [x1;x2;x3] -> (x1,x2,x3)
  | _ -> failwith "get3"

let get4 n lst =
  match lst with
    [x1;x2;x3;x4] -> (x1,x2,x3,x4)
  | _ -> failwith "get4"

let get1_0bound n x =
  match get1 n x with
    ([],z) -> z
  | _ -> failwith "get1_0bound"

let get1_1bound n x =
  match get1 n x with
    ([a],z) -> (a,z)
  | _ -> failwith "get1_1bound"

let get1_2bound n x =
  match get1 n x with
    ([a;b],z) -> (a,b,z)
  | _ -> failwith "get1_2bound"

let get2_0bound n x =
  match get2 n x with
    (([],z),([],w)) -> (z,w)
  | _ -> failwith "get2_0bound"

let get2_1bound n x =
  match get2 n x with
    (([a],z),([b],w)) -> (a,z,b,w)
  | _ -> failwith "get2_1bound"

let get2_02bound n x =
  match get2 n x with
    (([],z),([a;b],w)) -> (z,a,b,w)
  | _ -> failwith "get2_02bound"

let get2_03bound n x =
  match get2 n x with
    (([],z),([a;b;c],w)) -> (z,a,b,c,w)
  | _ -> failwith "get2_03bound"

let get3_202bound n x =
  match get3 n x with
    (([a;b],z),([],w),([c;d],y)) -> (a,b,z,w,c,d,y)
  | _ -> failwith "get3_202bound"
