
infix |>
fun x |> f = f x

infixr $
fun f $ x = f x

(* uninfixed composition of functions *)
fun comp f g = f o g

(* option type for being either Left of 'a or Right of b *)
datatype ('a, 'b) Either = Left of 'a | Right of 'b

(* gets the head of a list *)
exception noHeadOfEmptyList
fun head [] = raise noHeadOfEmptyList
  | head (x::_) = x

exception noTailOfEmptyList
fun tail [] = raise noTailOfEmptyList
  | tail (_::T) = T

exception emptyList
fun last [] = raise emptyList
  | last [x] = x
  | last L = last (tail L)

fun init [] = raise emptyList
  | init [x] = []
  | init (x::xs) = x::(init xs)

fun uncons [] = NONE
  | uncons (x::xs) = SOME(x, xs)

fun intersperse _ [] = []
  | intersperse y (x::xs) = x::y::(intersperse y xs)

val concat = List.map (op @)

fun intercalate y xs = concat (intersperse y xs)

fun nonEmptySubsequences [] = []
  | nonEmptySubsequences (x::xs) =
    let
      val res = nonEmptySubsequences xs
    in
      (List.map (fn a => x::a) res) @ res
    end

fun subsequences xs = []::(nonEmptySubsequences xs)


fun sort _ [] = []
  | sort _ [x] = []
  | sort cmp (x::xs) =
    let
      val left = List.filter (fn i => cmp(i, x) = LESS) xs
      val right = List.filter (fn i => cmp(i, x) <> LESS) xs
    in
      (sort cmp left) @ x::(sort cmp right)
    end



(* val null : 'a list -> bool
val length : 'a list -> int
val @ : 'a list * 'a list -> 'a list
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val last : 'a list -> 'a
val getItem : 'a list -> ('a * 'a list) option
val nth : 'a list * int -> 'a
val take : 'a list * int -> 'a list
val drop : 'a list * int -> 'a list
val rev : 'a list -> 'a list
val concat : 'a list list -> 'a list
val revAppend : 'a list * 'a list -> 'a list
val app : ('a -> unit) -> 'a list -> unit
val map : ('a -> 'b) -> 'a list -> 'b list
val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
val find : ('a -> bool) -> 'a list -> 'a option
val filter : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool)
                  -> 'a list -> 'a list * 'a list
val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val exists : ('a -> bool) -> 'a list -> bool
val all : ('a -> bool) -> 'a list -> bool
val tabulate : int * (int -> 'a) -> 'a list
val collate : ('a * 'a -> order)
                -> 'a list * 'a list -> order *)
