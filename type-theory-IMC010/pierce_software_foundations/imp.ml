type unit0 =
| Tt

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

type 'a list =
| Nil
| Cons of 'a * 'a list

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | Nil -> m
  | Cons (a, l1) -> Cons (a, (app l1 m))

(** val plus : int -> int -> int **)

let rec plus = ( + )

(** val mult : int -> int -> int **)

let rec mult = ( * )

(** val minus : int -> int -> int **)

let rec minus n0 m =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ ->
    n0)
    (fun k ->
    (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
      (fun _ ->
      n0)
      (fun l ->
      minus k l)
      m)
    n0

type positive =
| XI of positive
| XO of positive
| XH

(** val psucc : positive -> positive **)

let rec psucc = function
| XI p -> XO (psucc p)
| XO p -> XI p
| XH -> XO XH

(** val pplus : positive -> positive -> positive **)

let rec pplus x y =
  match x with
  | XI p ->
    (match y with
     | XI q -> XO (pplus_carry p q)
     | XO q -> XI (pplus p q)
     | XH -> XO (psucc p))
  | XO p ->
    (match y with
     | XI q -> XI (pplus p q)
     | XO q -> XO (pplus p q)
     | XH -> XI p)
  | XH ->
    (match y with
     | XI q -> XO (psucc q)
     | XO q -> XI q
     | XH -> XO XH)

(** val pplus_carry : positive -> positive -> positive **)

and pplus_carry x y =
  match x with
  | XI p ->
    (match y with
     | XI q -> XI (pplus_carry p q)
     | XO q -> XO (pplus_carry p q)
     | XH -> XI (psucc p))
  | XO p ->
    (match y with
     | XI q -> XO (pplus_carry p q)
     | XO q -> XI (pplus p q)
     | XH -> XO (psucc p))
  | XH ->
    (match y with
     | XI q -> XI (psucc q)
     | XO q -> XO (psucc q)
     | XH -> XI XH)

(** val pmult_nat : positive -> int -> int **)

let rec pmult_nat x pow2 =
  match x with
  | XI p -> plus pow2 (pmult_nat p (plus pow2 pow2))
  | XO p -> pmult_nat p (plus pow2 pow2)
  | XH -> pow2

(** val nat_of_P : positive -> int **)

let nat_of_P x =
  pmult_nat x ((fun x -> x + 1) 0)

(** val pmult : positive -> positive -> positive **)

let rec pmult x y =
  match x with
  | XI p -> pplus y (XO (pmult p y))
  | XO p -> XO (pmult p y)
  | XH -> y

type n =
| N0
| Npos of positive

(** val nplus : n -> n -> n **)

let nplus n0 m =
  match n0 with
  | N0 -> m
  | Npos p ->
    (match m with
     | N0 -> n0
     | Npos q -> Npos (pplus p q))

(** val nmult : n -> n -> n **)

let nmult n0 m =
  match n0 with
  | N0 -> N0
  | Npos p ->
    (match m with
     | N0 -> N0
     | Npos q -> Npos (pmult p q))

(** val beq_nat : int -> int -> bool **)

let rec beq_nat = ( = )

(** val nat_of_N : n -> int **)

let nat_of_N = function
| N0 -> 0
| Npos p -> nat_of_P p

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| Nil -> Nil
| Cons (x, l') -> app (rev l') (Cons (x, Nil))

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| Nil -> Nil
| Cons (a, t) -> Cons ((f a), (map f t))

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | Nil -> a0
  | Cons (b, t) -> fold_left f t (f a0 b)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| Nil -> a0
| Cons (b, t) -> f b (fold_right f a0 t)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| Nil -> true
| Cons (a, l0) -> if f a then forallb f l0 else false

(** val n_of_digits : bool list -> n **)

let rec n_of_digits = function
| Nil -> N0
| Cons (b, l') ->
  nplus (if b then Npos XH else N0) (nmult (Npos (XO XH)) (n_of_digits l'))

(** val n_of_ascii : char -> n **)

let n_of_ascii a =
  (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun a0 a1 a2 a3 a4 a5 a6 a7 ->
    n_of_digits (Cons (a0, (Cons (a1, (Cons (a2, (Cons (a3, (Cons (a4, (Cons
      (a5, (Cons (a6, (Cons (a7, Nil)))))))))))))))))
    a

(** val nat_of_ascii : char -> int **)

let nat_of_ascii a =
  nat_of_N (n_of_ascii a)

type string =
| EmptyString
| String of char * string

(** val string_dec : string -> string -> bool **)

let rec string_dec s s0 =
  match s with
  | EmptyString ->
    (match s0 with
     | EmptyString -> true
     | String (a, s1) -> false)
  | String (a, s1) ->
    (match s0 with
     | EmptyString -> false
     | String (a0, s2) -> if (=) a a0 then string_dec s1 s2 else false)

(** val append : string -> string -> string **)

let rec append s1 s2 =
  match s1 with
  | EmptyString -> s2
  | String (c, s1') -> String (c, (append s1' s2))

(** val ble_nat : int -> int -> bool **)

let rec ble_nat n0 m =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ ->
    true)
    (fun n' ->
    (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
      (fun _ ->
      false)
      (fun m' ->
      ble_nat n' m')
      m)
    n0

type id =
  int
  (* singleton inductive, whose constructor was Id *)

(** val beq_id : id -> id -> bool **)

let beq_id id1 id2 =
  beq_nat id1 id2

type state = id -> int

(** val empty_state : state **)

let empty_state x =
  0

(** val update : state -> id -> int -> state **)

let update st x n0 x' =
  if beq_id x x' then n0 else st x'

type aexp =
| ANum of int
| AId of id
| APlus of aexp * aexp
| AMinus of aexp * aexp
| AMult of aexp * aexp

type bexp =
| BTrue
| BFalse
| BEq of aexp * aexp
| BLe of aexp * aexp
| BNot of bexp
| BAnd of bexp * bexp

(** val aeval : state -> aexp -> int **)

let rec aeval st = function
| ANum n0 -> n0
| AId x -> st x
| APlus (a1, a2) -> plus (aeval st a1) (aeval st a2)
| AMinus (a1, a2) -> minus (aeval st a1) (aeval st a2)
| AMult (a1, a2) -> mult (aeval st a1) (aeval st a2)

(** val beval : state -> bexp -> bool **)

let rec beval st = function
| BTrue -> true
| BFalse -> false
| BEq (a1, a2) -> beq_nat (aeval st a1) (aeval st a2)
| BLe (a1, a2) -> ble_nat (aeval st a1) (aeval st a2)
| BNot b1 -> negb (beval st b1)
| BAnd (b1, b2) -> if beval st b1 then beval st b2 else false

type com =
| CSkip
| CAss of id * aexp
| CSeq of com * com
| CIf of bexp * com * com
| CWhile of bexp * com

(** val ceval_step : state -> com -> int -> state option **)

let rec ceval_step st c i =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ ->
    None)
    (fun i' ->
    match c with
    | CSkip -> Some st
    | CAss (l, a1) -> Some (update st l (aeval st a1))
    | CSeq (c1, c2) ->
      (match ceval_step st c1 i' with
       | Some st' -> ceval_step st' c2 i'
       | None -> None)
    | CIf (b, c1, c2) ->
      if beval st b then ceval_step st c1 i' else ceval_step st c2 i'
    | CWhile (b1, c1) ->
      if beval st b1
      then (match ceval_step st c1 i' with
            | Some st' -> ceval_step st' c i'
            | None -> None)
      else Some st)
    i

(** val isWhite : char -> bool **)

let isWhite c =
  let n0 = nat_of_ascii c in
  if if beq_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1)
          0))))))))))))))))))))))))))))))))
     then true
     else beq_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) 0)))))))))
  then true
  else if beq_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) 0))))))))))
       then true
       else beq_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) 0)))))))))))))

(** val isLowerAlpha : char -> bool **)

let isLowerAlpha c =
  let n0 = nat_of_ascii c in
  if ble_nat ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1)
       0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       n0
  then ble_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1)
         0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  else false

(** val isAlpha : char -> bool **)

let isAlpha c =
  let n0 = nat_of_ascii c in
  if if ble_nat ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
          ((fun x -> x + 1) ((fun x -> x + 1)
          0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          n0
     then ble_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     else false
  then true
  else if ble_nat ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
            ((fun x -> x + 1)
            0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
            n0
       then ble_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
              ((fun x -> x + 1) ((fun x -> x + 1)
              0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       else false

(** val isDigit : char -> bool **)

let isDigit c =
  let n0 = nat_of_ascii c in
  if ble_nat ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
       0)))))))))))))))))))))))))))))))))))))))))))))))) n0
  then ble_nat n0 ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
         0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  else false

type chartype =
| White
| Alpha
| Digit
| Other

(** val classifyChar : char -> chartype **)

let classifyChar c =
  if isWhite c
  then White
  else if isAlpha c then Alpha else if isDigit c then Digit else Other

(** val list_of_string : string -> char list **)

let rec list_of_string = function
| EmptyString -> Nil
| String (c, s0) -> Cons (c, (list_of_string s0))

(** val string_of_list : char list -> string **)

let rec string_of_list xs =
  fold_right (fun x x0 -> String (x, x0)) EmptyString xs

type token = string

(** val tokenize_helper :
    chartype -> char list -> char list -> char list list **)

let rec tokenize_helper cls acc xs =
  let tk =
    match acc with
    | Nil -> Nil
    | Cons (a, l) -> Cons ((rev acc), Nil)
  in
  (match xs with
   | Nil -> tk
   | Cons (x, xs') ->
     (match cls with
      | White ->
        (match classifyChar x with
         | White ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs')
             else if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs'))
             x
         | Other ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper Other (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Other (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Other (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Other
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Other
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Other (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Other (Cons (x, Nil))
                                     xs')
             else if b0
                  then app tk (tokenize_helper Other (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Other (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Other (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Other
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Other
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Other (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Other (Cons (x, Nil))
                                     xs'))
             x
         | x0 ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                  else if b1
                       then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper x0 (Cons (x, Nil))
                                          xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper x0 (Cons
                                                    (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper x0
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper x0 (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper x0 (Cons (x, Nil)) xs')
             else if b0
                  then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                  else if b1
                       then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper x0 (Cons (x, Nil))
                                          xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper x0 (Cons
                                                    (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper x0
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper x0 (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper x0 (Cons (x, Nil)) xs'))
             x)
      | Alpha ->
        (match classifyChar x with
         | White ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs')
             else if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs'))
             x
         | Alpha ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then tokenize_helper Alpha (Cons (x, acc)) xs'
                  else if b1
                       then tokenize_helper Alpha (Cons (x, acc)) xs'
                       else if b2
                            then if b3
                                 then tokenize_helper Alpha (Cons (x, acc))
                                        xs'
                                 else if b4
                                      then if b5
                                           then tokenize_helper Alpha (Cons
                                                  (x, acc)) xs'
                                           else if b6
                                                then tokenize_helper Alpha
                                                       (Cons (x, acc)) xs'
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else tokenize_helper Alpha (Cons (x,
                                             acc)) xs'
                            else tokenize_helper Alpha (Cons (x, acc)) xs'
             else if b0
                  then tokenize_helper Alpha (Cons (x, acc)) xs'
                  else if b1
                       then tokenize_helper Alpha (Cons (x, acc)) xs'
                       else if b2
                            then if b3
                                 then tokenize_helper Alpha (Cons (x, acc))
                                        xs'
                                 else if b4
                                      then if b5
                                           then tokenize_helper Alpha (Cons
                                                  (x, acc)) xs'
                                           else if b6
                                                then tokenize_helper Alpha
                                                       (Cons (x, acc)) xs'
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else tokenize_helper Alpha (Cons (x,
                                             acc)) xs'
                            else tokenize_helper Alpha (Cons (x, acc)) xs')
             x
         | Digit ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper Digit (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Digit (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Digit (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Digit
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Digit
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Digit (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Digit (Cons (x, Nil))
                                     xs')
             else if b0
                  then app tk (tokenize_helper Digit (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Digit (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Digit (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Digit
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Digit
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Digit (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Digit (Cons (x, Nil))
                                     xs'))
             x
         | Other ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper Other (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Other (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Other (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Other
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Other
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Other (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Other (Cons (x, Nil))
                                     xs')
             else if b0
                  then app tk (tokenize_helper Other (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Other (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Other (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Other
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Other
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Other (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Other (Cons (x, Nil))
                                     xs'))
             x)
      | Digit ->
        (match classifyChar x with
         | White ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs')
             else if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs'))
             x
         | Alpha ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper Alpha (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Alpha (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Alpha (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Alpha
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Alpha
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Alpha (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Alpha (Cons (x, Nil))
                                     xs')
             else if b0
                  then app tk (tokenize_helper Alpha (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Alpha (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Alpha (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Alpha
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Alpha
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Alpha (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Alpha (Cons (x, Nil))
                                     xs'))
             x
         | Digit ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then tokenize_helper Digit (Cons (x, acc)) xs'
                  else if b1
                       then tokenize_helper Digit (Cons (x, acc)) xs'
                       else if b2
                            then if b3
                                 then tokenize_helper Digit (Cons (x, acc))
                                        xs'
                                 else if b4
                                      then if b5
                                           then tokenize_helper Digit (Cons
                                                  (x, acc)) xs'
                                           else if b6
                                                then tokenize_helper Digit
                                                       (Cons (x, acc)) xs'
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else tokenize_helper Digit (Cons (x,
                                             acc)) xs'
                            else tokenize_helper Digit (Cons (x, acc)) xs'
             else if b0
                  then tokenize_helper Digit (Cons (x, acc)) xs'
                  else if b1
                       then tokenize_helper Digit (Cons (x, acc)) xs'
                       else if b2
                            then if b3
                                 then tokenize_helper Digit (Cons (x, acc))
                                        xs'
                                 else if b4
                                      then if b5
                                           then tokenize_helper Digit (Cons
                                                  (x, acc)) xs'
                                           else if b6
                                                then tokenize_helper Digit
                                                       (Cons (x, acc)) xs'
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else tokenize_helper Digit (Cons (x,
                                             acc)) xs'
                            else tokenize_helper Digit (Cons (x, acc)) xs')
             x
         | Other ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper Other (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Other (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Other (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Other
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Other
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Other (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Other (Cons (x, Nil))
                                     xs')
             else if b0
                  then app tk (tokenize_helper Other (Cons (x, Nil)) xs')
                  else if b1
                       then app tk
                              (tokenize_helper Other (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper Other (Cons (x,
                                          Nil)) xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper Other
                                                    (Cons (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper Other
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper Other (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper Other (Cons (x, Nil))
                                     xs'))
             x)
      | Other ->
        (match classifyChar x with
         | White ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs')
             else if b0
                  then app tk (tokenize_helper White Nil xs')
                  else if b1
                       then app tk (tokenize_helper White Nil xs')
                       else if b2
                            then if b3
                                 then app tk (tokenize_helper White Nil xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper White Nil
                                                    xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper White
                                                         Nil xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper White Nil xs')
                            else app tk (tokenize_helper White Nil xs'))
             x
         | Other ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then tokenize_helper Other (Cons (x, acc)) xs'
                  else if b1
                       then tokenize_helper Other (Cons (x, acc)) xs'
                       else if b2
                            then if b3
                                 then tokenize_helper Other (Cons (x, acc))
                                        xs'
                                 else if b4
                                      then if b5
                                           then tokenize_helper Other (Cons
                                                  (x, acc)) xs'
                                           else if b6
                                                then tokenize_helper Other
                                                       (Cons (x, acc)) xs'
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else tokenize_helper Other (Cons (x,
                                             acc)) xs'
                            else tokenize_helper Other (Cons (x, acc)) xs'
             else if b0
                  then tokenize_helper Other (Cons (x, acc)) xs'
                  else if b1
                       then tokenize_helper Other (Cons (x, acc)) xs'
                       else if b2
                            then if b3
                                 then tokenize_helper Other (Cons (x, acc))
                                        xs'
                                 else if b4
                                      then if b5
                                           then tokenize_helper Other (Cons
                                                  (x, acc)) xs'
                                           else if b6
                                                then tokenize_helper Other
                                                       (Cons (x, acc)) xs'
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else tokenize_helper Other (Cons (x,
                                             acc)) xs'
                            else tokenize_helper Other (Cons (x, acc)) xs')
             x
         | x0 ->
           (* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
             (fun b b0 b1 b2 b3 b4 b5 b6 ->
             if b
             then if b0
                  then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                  else if b1
                       then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper x0 (Cons (x, Nil))
                                          xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper x0 (Cons
                                                    (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper x0
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       (')', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper x0 (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper x0 (Cons (x, Nil)) xs')
             else if b0
                  then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                  else if b1
                       then app tk (tokenize_helper x0 (Cons (x, Nil)) xs')
                       else if b2
                            then if b3
                                 then app tk
                                        (tokenize_helper x0 (Cons (x, Nil))
                                          xs')
                                 else if b4
                                      then if b5
                                           then app tk
                                                  (tokenize_helper x0 (Cons
                                                    (x, Nil)) xs')
                                           else if b6
                                                then app tk
                                                       (tokenize_helper x0
                                                         (Cons (x, Nil)) xs')
                                                else app tk (Cons ((Cons
                                                       ('(', Nil)),
                                                       (tokenize_helper Other
                                                         Nil xs')))
                                      else app tk
                                             (tokenize_helper x0 (Cons (x,
                                               Nil)) xs')
                            else app tk
                                   (tokenize_helper x0 (Cons (x, Nil)) xs'))
             x)))

(** val tokenize : string -> string list **)

let tokenize s =
  map string_of_list (tokenize_helper White Nil (list_of_string s))

type 'x optionE =
| SomeE of 'x
| NoneE of string

(** val build_symtable : token list -> int -> token -> int **)

let rec build_symtable xs n0 =
  match xs with
  | Nil -> (fun s -> n0)
  | Cons (x, xs0) ->
    if forallb isLowerAlpha (list_of_string x)
    then (fun s ->
           if string_dec s x
           then n0
           else build_symtable xs0 ((fun x -> x + 1) n0) s)
    else build_symtable xs0 n0

type 't parser0 = token list -> ('t, token list) prod optionE

(** val many_helper :
    'a1 parser0 -> 'a1 list -> int -> token list -> ('a1 list, token list)
    prod optionE **)

let rec many_helper p acc steps xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match p xs with
    | SomeE p0 ->
      let Pair (t, xs') = p0 in many_helper p (Cons (t, acc)) steps' xs'
    | NoneE s -> SomeE (Pair ((rev acc), xs)))
    steps

(** val many : 'a1 parser0 -> int -> 'a1 list parser0 **)

let rec many p steps =
  many_helper p Nil steps

(** val firstExpect : token -> 'a1 parser0 -> 'a1 parser0 **)

let firstExpect t p = function
| Nil ->
  NoneE
    (append (String ('e', (String ('x', (String ('p', (String ('e', (String
      ('c', (String ('t', (String ('e', (String ('d', (String (' ', (String
      ('\'', EmptyString))))))))))))))))))))
      (append t (String ('\'', (String ('.', EmptyString))))))
| Cons (x, xs') ->
  if string_dec x t
  then p xs'
  else NoneE
         (append (String ('e', (String ('x', (String ('p', (String ('e',
           (String ('c', (String ('t', (String ('e', (String ('d', (String
           (' ', (String ('\'', EmptyString))))))))))))))))))))
           (append t (String ('\'', (String ('.', EmptyString))))))

(** val expect : token -> unit0 parser0 **)

let expect t =
  firstExpect t (fun xs -> SomeE (Pair (Tt, xs)))

(** val parseIdentifier :
    (string -> int) -> token list -> (id, token list) prod optionE **)

let parseIdentifier symtable = function
| Nil ->
  NoneE (String ('E', (String ('x', (String ('p', (String ('e', (String ('c',
    (String ('t', (String ('e', (String ('d', (String (' ', (String ('i',
    (String ('d', (String ('e', (String ('n', (String ('t', (String ('i',
    (String ('f', (String ('i', (String ('e', (String ('r',
    EmptyString))))))))))))))))))))))))))))))))))))))
| Cons (x, xs') ->
  if forallb isLowerAlpha (list_of_string x)
  then SomeE (Pair ((symtable x), xs'))
  else NoneE
         (append (String ('I', (String ('l', (String ('l', (String ('e',
           (String ('g', (String ('a', (String ('l', (String (' ', (String
           ('i', (String ('d', (String ('e', (String ('n', (String ('t',
           (String ('i', (String ('f', (String ('i', (String ('e', (String
           ('r', (String (':', (String ('\'',
           EmptyString))))))))))))))))))))))))))))))))))))))))
           (append x (String ('\'', EmptyString))))

(** val parseNumber : token list -> (int, token list) prod optionE **)

let parseNumber = function
| Nil ->
  NoneE (String ('E', (String ('x', (String ('p', (String ('e', (String ('c',
    (String ('t', (String ('e', (String ('d', (String (' ', (String ('n',
    (String ('u', (String ('m', (String ('b', (String ('e', (String ('r',
    EmptyString))))))))))))))))))))))))))))))
| Cons (x, xs') ->
  if forallb isDigit (list_of_string x)
  then SomeE (Pair
         ((fold_left (fun n0 d ->
            plus
              (mult ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
                ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
                ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
                ((fun x -> x + 1) 0)))))))))) n0)
              (minus (nat_of_ascii d) (nat_of_ascii '0'))) (list_of_string x)
            0), xs'))
  else NoneE (String ('E', (String ('x', (String ('p', (String ('e', (String
         ('c', (String ('t', (String ('e', (String ('d', (String (' ',
         (String ('n', (String ('u', (String ('m', (String ('b', (String
         ('e', (String ('r', EmptyString))))))))))))))))))))))))))))))

(** val parsePrimaryExp :
    int -> (string -> int) -> token list -> (aexp, token list) prod optionE **)

let rec parsePrimaryExp steps symtable xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match parseIdentifier symtable xs with
    | SomeE p -> let Pair (i, rest) = p in SomeE (Pair ((AId i), rest))
    | NoneE err ->
      (match parseNumber xs with
       | SomeE p -> let Pair (n0, rest) = p in SomeE (Pair ((ANum n0), rest))
       | NoneE err0 ->
         (match firstExpect (String ('(', EmptyString))
                  (parseSumExp steps' symtable) xs with
          | SomeE p ->
            let Pair (e, rest) = p in
            (match expect (String (')', EmptyString)) rest with
             | SomeE p0 ->
               let Pair (u, rest') = p0 in SomeE (Pair (e, rest'))
             | NoneE err1 -> NoneE err1)
          | NoneE err1 -> NoneE err1)))
    steps

(** val parseProductExp :
    int -> (string -> int) -> token list -> (aexp, token list) prod optionE **)

and parseProductExp steps symtable xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match parsePrimaryExp steps' symtable xs with
    | SomeE p ->
      let Pair (e, rest) = p in
      (match many
               (firstExpect (String ('*', EmptyString))
                 (parsePrimaryExp steps' symtable)) steps' rest with
       | SomeE p0 ->
         let Pair (es, rest') = p0 in
         SomeE (Pair ((fold_left (fun x x0 -> AMult (x, x0)) es e), rest'))
       | NoneE err -> NoneE err)
    | NoneE err -> NoneE err)
    steps

(** val parseSumExp :
    int -> (string -> int) -> token list -> (aexp, token list) prod optionE **)

and parseSumExp steps symtable xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match parseProductExp steps' symtable xs with
    | SomeE p ->
      let Pair (e, rest) = p in
      (match many (fun xs0 ->
               match firstExpect (String ('+', EmptyString))
                       (parseProductExp steps' symtable) xs0 with
               | SomeE p0 ->
                 let Pair (e0, rest') = p0 in
                 SomeE (Pair ((Pair (true, e0)), rest'))
               | NoneE err ->
                 (match firstExpect (String ('-', EmptyString))
                          (parseProductExp steps' symtable) xs0 with
                  | SomeE p0 ->
                    let Pair (e0, rest') = p0 in
                    SomeE (Pair ((Pair (false, e0)), rest'))
                  | NoneE err0 -> NoneE err0)) steps' rest with
       | SomeE p0 ->
         let Pair (es, rest') = p0 in
         SomeE (Pair
         ((fold_left (fun e0 term ->
            let Pair (y, e1) = term in
            if y then APlus (e0, e1) else AMinus (e0, e1)) es e), rest'))
       | NoneE err -> NoneE err)
    | NoneE err -> NoneE err)
    steps

(** val parseAExp :
    int -> (string -> int) -> token list -> (aexp, token list) prod optionE **)

let parseAExp =
  parseSumExp

(** val parseAtomicExp :
    int -> (string -> int) -> token list -> (bexp, token list) prod optionE **)

let rec parseAtomicExp steps symtable xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match expect (String ('t', (String ('r', (String ('u', (String ('e',
            EmptyString)))))))) xs with
    | SomeE p -> let Pair (u, rest) = p in SomeE (Pair (BTrue, rest))
    | NoneE err ->
      (match expect (String ('f', (String ('a', (String ('l', (String ('s',
               (String ('e', EmptyString)))))))))) xs with
       | SomeE p -> let Pair (u, rest) = p in SomeE (Pair (BFalse, rest))
       | NoneE err0 ->
         (match firstExpect (String ('n', (String ('o', (String ('t',
                  EmptyString)))))) (parseAtomicExp steps' symtable) xs with
          | SomeE p ->
            let Pair (e, rest) = p in SomeE (Pair ((BNot e), rest))
          | NoneE err1 ->
            (match firstExpect (String ('(', EmptyString))
                     (parseConjunctionExp steps' symtable) xs with
             | SomeE p ->
               let Pair (e, rest) = p in
               (match expect (String (')', EmptyString)) rest with
                | SomeE p0 ->
                  let Pair (u, rest') = p0 in SomeE (Pair (e, rest'))
                | NoneE err2 -> NoneE err2)
             | NoneE err2 ->
               (match parseProductExp steps' symtable xs with
                | SomeE p ->
                  let Pair (e, rest) = p in
                  (match firstExpect (String ('=', (String ('=',
                           EmptyString)))) (parseAExp steps' symtable) rest with
                   | SomeE p0 ->
                     let Pair (e', rest') = p0 in
                     SomeE (Pair ((BEq (e, e')), rest'))
                   | NoneE err3 ->
                     (match firstExpect (String ('<', (String ('=',
                              EmptyString)))) (parseAExp steps' symtable)
                              rest with
                      | SomeE p0 ->
                        let Pair (e', rest') = p0 in
                        SomeE (Pair ((BLe (e, e')), rest'))
                      | NoneE err4 ->
                        NoneE (String ('E', (String ('x', (String ('p',
                          (String ('e', (String ('c', (String ('t', (String
                          ('e', (String ('d', (String (' ', (String ('\'',
                          (String ('=', (String ('=', (String ('\'', (String
                          (' ', (String ('o', (String ('r', (String (' ',
                          (String ('\'', (String ('<', (String ('=', (String
                          ('\'', (String (' ', (String ('a', (String ('f',
                          (String ('t', (String ('e', (String ('r', (String
                          (' ', (String ('a', (String ('r', (String ('i',
                          (String ('t', (String ('h', (String ('m', (String
                          ('e', (String ('t', (String ('i', (String ('c',
                          (String (' ', (String ('e', (String ('x', (String
                          ('p', (String ('r', (String ('e', (String ('s',
                          (String ('s', (String ('i', (String ('o', (String
                          ('n',
                          EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                | NoneE err3 -> NoneE err3)))))
    steps

(** val parseConjunctionExp :
    int -> (string -> int) -> token list -> (bexp, token list) prod optionE **)

and parseConjunctionExp steps symtable xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match parseAtomicExp steps' symtable xs with
    | SomeE p ->
      let Pair (e, rest) = p in
      (match many
               (firstExpect (String ('&', (String ('&', EmptyString))))
                 (parseAtomicExp steps' symtable)) steps' rest with
       | SomeE p0 ->
         let Pair (es, rest') = p0 in
         SomeE (Pair ((fold_left (fun x x0 -> BAnd (x, x0)) es e), rest'))
       | NoneE err -> NoneE err)
    | NoneE err -> NoneE err)
    steps

(** val parseBExp :
    int -> (string -> int) -> token list -> (bexp, token list) prod optionE **)

let parseBExp =
  parseConjunctionExp

(** val parseSimpleCommand :
    int -> (string -> int) -> token list -> (com, token list) prod optionE **)

let rec parseSimpleCommand steps symtable xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match expect (String ('S', (String ('K', (String ('I', (String ('P',
            EmptyString)))))))) xs with
    | SomeE p -> let Pair (u, rest) = p in SomeE (Pair (CSkip, rest))
    | NoneE err ->
      (match firstExpect (String ('I', (String ('F', EmptyString))))
               (parseBExp steps' symtable) xs with
       | SomeE p ->
         let Pair (e, rest) = p in
         (match firstExpect (String ('T', (String ('H', (String ('E', (String
                  ('N', EmptyString))))))))
                  (parseSequencedCommand steps' symtable) rest with
          | SomeE p0 ->
            let Pair (c, rest') = p0 in
            (match firstExpect (String ('E', (String ('L', (String ('S',
                     (String ('E', EmptyString))))))))
                     (parseSequencedCommand steps' symtable) rest' with
             | SomeE p1 ->
               let Pair (c', rest'') = p1 in
               (match expect (String ('E', (String ('N', (String ('D',
                        EmptyString)))))) rest'' with
                | SomeE p2 ->
                  let Pair (u, rest''') = p2 in
                  SomeE (Pair ((CIf (e, c, c')), rest'''))
                | NoneE err0 -> NoneE err0)
             | NoneE err0 -> NoneE err0)
          | NoneE err0 -> NoneE err0)
       | NoneE err0 ->
         (match firstExpect (String ('W', (String ('H', (String ('I', (String
                  ('L', (String ('E', EmptyString))))))))))
                  (parseBExp steps' symtable) xs with
          | SomeE p ->
            let Pair (e, rest) = p in
            (match firstExpect (String ('D', (String ('O', EmptyString))))
                     (parseSequencedCommand steps' symtable) rest with
             | SomeE p0 ->
               let Pair (c, rest') = p0 in
               (match expect (String ('E', (String ('N', (String ('D',
                        EmptyString)))))) rest' with
                | SomeE p1 ->
                  let Pair (u, rest'') = p1 in
                  SomeE (Pair ((CWhile (e, c)), rest''))
                | NoneE err1 -> NoneE err1)
             | NoneE err1 -> NoneE err1)
          | NoneE err1 ->
            (match parseIdentifier symtable xs with
             | SomeE p ->
               let Pair (i, rest) = p in
               (match firstExpect (String (':', (String ('=', EmptyString))))
                        (parseAExp steps' symtable) rest with
                | SomeE p0 ->
                  let Pair (e, rest') = p0 in
                  SomeE (Pair ((CAss (i, e)), rest'))
                | NoneE err2 -> NoneE err2)
             | NoneE err2 -> NoneE err2))))
    steps

(** val parseSequencedCommand :
    int -> (string -> int) -> token list -> (com, token list) prod optionE **)

and parseSequencedCommand steps symtable xs =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ -> NoneE (String ('T', (String ('o', (String ('o', (String (' ',
    (String ('m', (String ('a', (String ('n', (String ('y', (String (' ',
    (String ('r', (String ('e', (String ('c', (String ('u', (String ('r',
    (String ('s', (String ('i', (String ('v', (String ('e', (String (' ',
    (String ('c', (String ('a', (String ('l', (String ('l', (String ('s',
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun steps' ->
    match parseSimpleCommand steps' symtable xs with
    | SomeE p ->
      let Pair (c, rest) = p in
      (match firstExpect (String (';', EmptyString))
               (parseSequencedCommand steps' symtable) rest with
       | SomeE p0 ->
         let Pair (c', rest') = p0 in SomeE (Pair ((CSeq (c, c')), rest'))
       | NoneE err -> SomeE (Pair (c, rest)))
    | NoneE err -> NoneE err)
    steps

(** val bignumber : int **)

let bignumber =
  (fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1) ((fun x -> x + 1)
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val parse : string -> (com, token list) prod optionE **)

let parse str =
  let tokens = tokenize str in
  parseSequencedCommand bignumber (build_symtable tokens 0) tokens

