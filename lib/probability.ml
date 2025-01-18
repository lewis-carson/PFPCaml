(* 
  0 AUXILIARY DEFINITIONS
  1 DETERMINISTIC AND PROBABILISTIC VALUES
  2 RANDOMIZED VALUES
  3 DETERMINISTIC AND PROBABILISTIC GENERATORS
  4 RANDOMIZED GENERATORS
  5 ITERATORS AND SIMULATORS
  6 TRACING
*)

(* 0 AUXILIARY DEFINITIONS *)

(* Declares a function type for events on 'a. *)
type 'a event = 'a -> bool

(* Checks if 'x' appears in the list 'lst'. *)
let one_of lst x = List.mem x lst

(* Uses 'one_of' with a single-element list. *)
let just x = one_of [x]

(* Defines a 'probability' type wrapper around float. *)
type probability = P of float

(* Determines how many decimal places are used for formatting. *)
let precision = 1

(* Formats a float as a percentage string. *)
let show_pfix f =
  if precision = 0 then
    Printf.sprintf "%.0f%%" (f *. 100.0)
  else
    let d = 10.0 ** float_of_int precision in
    Printf.sprintf "%.*f%%" (4 + precision) (floor (f *. 100.0 *. d) /. d)

(* Binds 'show_p' function to 'show_pfix'. *)
let show_p = show_pfix

(* Small threshold for floating-point comparisons. *)
let error_margin = 0.00001

(* Returns a simple option type wrapping 'x'. *)
let return x = Some x

(* 'bind' operator for chaining optional computations. *)
let ( >>= ) m f =
  match m with
  | None -> None
  | Some x -> f x

(* Composition operator for functions returning option. *)
let ( >@> ) f g x = f x >>= g

(* Sequentially apply a list of functions. *)
let sequ fs x = List.fold_left ( >@> ) return fs x

(* 1 DETERMINISTIC AND PROBABILISTIC VALUES *)

(* Defines a distribution type as a list of (value, probability) pairs. *)
type 'a dist = D of ('a * float) list

(* Returns a distribution with one value at probability 1.0. *)
let return x = D [(x, 1.0)]

(* Monadic bind for distributions. *)
let bind (D d) f =
  D (List.concat (List.map (fun (x, p) -> let D d' = f x in List.map (fun (y, q) -> (y, p *. q)) d') d))

(* Returns an empty distribution. *)
let fail _ = D []

(* Checks if distribution is empty. *)
let is_zero (D d) = d = []

(* Combines two distributions unless one is empty. *)
let mplus (D d1) (D d2) =
  if is_zero (D d1) || is_zero (D d2) then D []
  else D (d1 @ d2)

(* Applies function 'f' to each value in a distribution. *)
let fmap f (D d) = D (List.map (fun (x, p) -> (f x, p)) d)

(* Normalizes distribution by a grouping function. *)
let norm_by f (D d) =
  let rec accum_by f = function
    | (x, p) :: (y, q) :: xs when f x y -> accum_by f ((x, p +. q) :: xs)
    | x :: xs -> x :: accum_by f xs
    | [] -> []
  in
  D (accum_by f (List.sort (fun (x, _) (y, _) -> compare x y) d))

(* Normalizes distribution by equality of sampled values. *)
let norm d = norm_by (=) d

(* Renders distribution for printing. *)
let show_dist show_a (D d) =
  if d = [] then "Impossible"
  else
    let sorted_d = List.sort (fun (_, p1) (_, p2) -> compare p2 p1) d in
    String.concat "\n" (List.map (fun (x, p) -> Printf.sprintf "%s %s" (show_a x) (show_p p)) sorted_d)

(* Joins two distributions using a pairing function. *)
let join_with f (D d1) (D d2) =
  D (List.concat (List.map (fun (x, p) -> List.map (fun (y, q) -> (f x y, p *. q)) d2) d1))

(* Creates a distribution of pairs from two distributions. *)
let prod d1 d2 = join_with (fun x y -> (x, y)) d1 d2

(* Returns a certain distribution of x. *)
let certainly x = return x

(* Represents an impossible event. *)
let impossible = D []

(* Chooses between x and y with probability p. *)
let choose p x y = D [(x, p); (y, 1.0 -. p)]

(* Creates a distribution from corresponding lists of probabilities and values. *)
let enum ps xs = D (List.combine xs ps)

(* Creates a distribution from probabilities in percent. *)
let enumPC ps xs = enum (List.map (fun p -> p /. 100.0) ps) xs

(* Creates a distribution from relative frequencies. *)
let relative ns = enum (List.map (fun n -> float_of_int n /. float_of_int (List.fold_left (+) 0 ns)) ns) (List.init (List.length ns) (fun i -> i))

(* Shapes a distribution of xs by applying f to a normalized index. *)
let shape (f : float -> float) (xs : float list) : float dist = 
  if xs = [] then impossible
  else
    let incr = 1.0 /. float_of_int ((List.length xs) - 1) in
    let ps = List.map f (List.init (List.length xs) (fun i -> float_of_int i *. incr)) in
    D (List.combine xs ps)

(* Creates a uniform distribution from xs. *)
let uniform_float (xs : float list) : float dist = shape (fun _ -> 1.0) xs

(* Creates a uniform distribution for any 'a list. *)
let uniform (xs : 'a list) : 'a dist =
  if xs = [] then impossible
  else
    let p = 1.0 /. float_of_int (List.length xs) in
    D (List.map (fun x -> (x, p)) xs)

(* Exponential distribution shaped by exp(-x). *)
let negexp (xs : float list) : float dist = shape (fun x -> exp (-. x)) xs

(* A normal curve function using mean and standard deviation. *)
let normalCurve mean stddev x = 1.0 /. sqrt (2.0 *. 3.14159) *. exp (-. 0.5 *. ((x -. mean) /. stddev) ** 2.0)

(* A normal distribution centered at 0.5 with stddev 0.5. *)
let normal (xs : float list) : float dist = shape (fun x -> normalCurve 0.5 0.5 x) xs

(* Extracts the domain of a distribution. *)
let extract (D d) = List.map fst d

(* Maps 'f' over a distribution's values. *)
let map_dist f (D d) = D (List.map (fun (x, p) -> (f x, p)) d)

(* Unfolds a distribution of distributions into one distribution. *)
let unfold_dist (D d) =
  D (List.concat (List.map (fun (D d', q) -> List.map (fun (x, p) -> (x, p *. q)) d') d))

(* Chooses between d1 and d2 by a boolean distribution b. *)
let cond b d1 d2 =
  let D d = choose (match b with D [(true, p); (false, _)] -> p | _ -> 0.5) d1 d2 in
  unfold_dist (D d)

(* Filters a distribution by predicate p, renormalizing. *)
let filter_dist p (D d) =
  let filtered = List.filter (fun (x, _) -> p x) d in
  let total_prob = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 filtered in
  D (List.map (fun (x, p) -> (x, p /. total_prob)) filtered)

(* Infix filter operator for distributions. *)
let (|||) d p = filter_dist p d

(* Selects a value from the distribution d by random float p. *)
let select_p (D d) p =
  let rec aux p = function
    | (x, q) :: _ when p <= q -> x
    | (_, q) :: xs -> aux (p -. q) xs
    | [] -> failwith "select_p: probability out of range"
  in
  aux p d

(* Infix operator to sum up probabilities of values that satisfy p. *)
let (??) p (D d) = P (List.fold_left (fun acc (x, q) -> if p x then acc +. q else acc) 0.0 d)

(* Declares an interface for converting to float. *)
class type to_float = object
  method to_float : float
end

(* Declares an interface for converting from float. *)
class type from_float = object
  method from_float : float -> 'a
end

(* Declares a module type for computing expected value. *)
module type Expected = sig
  type t
  val expected : t -> float
end

(* Calculates expected value of a distribution. *)
let expected (type a) (module E : Expected with type t = a) (D ps : a dist) =
  List.fold_left (fun acc (x, p) -> acc +. p *. E.expected x) 0.0 ps

(* Calculates variance of a distribution. *)
let variance (type a) (module E : Expected with type t = a) (D ps : a dist) =
  let ex = expected (module E) (D ps) in
  List.fold_left (fun acc (x, p) -> acc +. p *. (E.expected x -. ex) ** 2.0) 0.0 ps

(* Calculates standard deviation of a distribution. *)
let stddev (type a) (module E : Expected with type t = a) (d : a dist) =
  sqrt (variance (module E) d)

(* Defines a module for expected values of floats. *)
module ExpectedFloat = struct
  type t = float
  let expected x = x
end

(* Defines a module for expected values of ints. *)
module ExpectedInt = struct
  type t = int
  let expected x = float_of_int x
end

(* Another module for expected values of ints. *)
module ExpectedInteger = struct
  type t = int
  let expected x = float_of_int x
end

(* Defines a module for expected values of lists. *)
module ExpectedList (E : Expected) = struct
  type t = E.t list
  let expected xs = 
    let sum = List.fold_left (fun acc x -> acc +. E.expected x) 0.0 xs in
    sum /. float_of_int (List.length xs)
end

(* Defines a module for expected values of distributions. *)
module ExpectedDist (E : Expected) = struct
  type t = E.t dist
  let expected d = expected (module E) d
end

(* 2 RANDOMIZED VALUES *)

(* Random values *)
type 'a random = unit -> 'a

(* Prints a random value by applying 'r'. *)
let print_random r = Printf.printf "%s\n" (r ())

(* Picks a single value by random in distribution d. *)
let pick (D d) =
  let p = Random.float 1.0 in
  let rec aux p = function
    | (x, q) :: _ when p <= q -> x
    | (_, q) :: xs -> aux (p -. q) xs
    | [] -> failwith "pick: probability out of range"
  in
  aux p d

(* Randomized distributions *)
type 'a rdist = unit -> 'a dist

(* Example for cases in distributions. *)
type 'a case = Case of 'a | Other

(* Filters distribution by p, returning 'Case' or 'Other'. *)
let r_above p rd () =
  let D d = rd () in
  let sorted_d = List.sort (fun (_, q1) (_, q2) -> compare q2 q1) d in
  let rec span f = function
    | x :: xs when f x -> let (ys, zs) = span f xs in (x :: ys, zs)
    | xs -> ([], xs)
  in
  let (d1, d2) = span (fun (_, q) -> q >= p) sorted_d in
  D (List.map (fun (x, q) -> (Case x, q)) d1 @ [(Other, List.fold_left (fun acc (_, q) -> acc +. q) 0.0 d2)])

(* 3 DETERMINISTIC AND PROBABILISTIC GENERATORS *)

(* Transition types for changes resulting in distributions. *)
type 'a change = 'a -> 'a

(* Transition maps 'a to 'a dist. *)
type 'a trans = 'a -> 'a dist

(* Identity transition. *)
let id_trans x = certainly x

(* Maps a function f over the result of transition t. *)
let map_trans f t x = map_dist f (t x)

(* Unfolds a distribution of transitions. *)
let unfold_trans (D d) x =
  D (List.concat (List.map (fun (f, p) -> let D d' = f x in List.map (fun (y, q) -> (y, p *. q)) d') d))

(* Type for applying a list of changes to produce a trans. *)
type 'a spread_c = 'a change list -> 'a trans

(* Applies a change f with certainty. *)
let certainly_trans f x = certainly (f x)

(* Applies a change f with probability p. *)
let maybe_trans p f x = choose p (f x) x

(* Lifts a list of changes into a single distribution using s. *)
let lift_c s cs x = s (List.map (fun f -> f x) cs)

(* Uniformly picks one of the changes. *)
let uniform_trans cs x = lift_c uniform cs x

(* Normalizes a list of changes to ensure they sum up to 1.0. *)
let normalize_changes cs =
  let total = List.fold_left (fun acc _ -> acc +. 1.0) 0.0 cs in
  let weights = List.map (fun _ -> 1.0 /. total) cs in
  List.combine cs weights

(* Lifts changes with normalized weights. *)
let normal_trans cs x = lift_c normalize_changes cs x

(* Creates linear weights for changes. *)
let linear c xs =
  let total = List.fold_left (fun acc _ -> acc +. c) 0.0 xs in
  let weights = List.map (fun _ -> c /. total) xs in
  List.combine xs weights

(* Lifts changes with linear weighting. *)
let linear_trans c cs x = lift_c (linear c) cs x

(* Assigns probabilities ps to each change. *)
let enum_trans ps cs x = lift_c (enum ps) cs x

(* Type for combining multiple transitions into one. *)
type 'a spread_t = 'a trans list -> 'a trans

(* Unfolds transitions with s. *)
let lift_t s ts x = unfold_trans (s ts) x

(* Applies uniform weighting to a list of transitions. *)
let uniform_tt ts x = lift_t uniform ts x

(* Normal weighting for transitions. *)
let normal_tt ts x = lift_t norm ts x

(* Creates linear weighting for transitions. *)
let linear c ts =
  let total = List.fold_left (fun acc _ -> acc +. c) 0.0 ts in
  let weights = List.map (fun _ -> c /. total) ts in
  enum weights ts

(* Applies linear weighting with factor c. *)
let linear_tt c ts x = lift_t (linear c) ts x

(* Enumerates transitions with assigned probabilities. *)
let enum_tt ps ts x = lift_t (enum ps) ts x

(* 4 RANDOMIZED GENERATORS *)

(* Defines a random change from 'a -> 'a random. *)
type 'a rchange = 'a -> 'a random

(* Applies 'pick' to the result of t x. *)
let random t x = pick (t x)

(* Defines a random transition type. *)
type 'a rtrans = 'a -> 'a rdist

(* Defines approximations of distributions from random values. *)
type 'a approx_dist = 'a random list -> 'a rdist

(* Converts a list of random values into a normalized distribution. *)
let rdist rs () =
  let values = List.map (fun r -> r ()) rs in
  norm (uniform values)

(* 5 ITERATORS AND SIMULATORS *)

(* Iterate class defining *. *)
module type Iterate = sig
  type 'a t
  val iter : int -> ('a -> 'a t) -> ('a -> 'a t)
  val while_ : ('a -> bool) -> ('a -> 'a t) -> ('a -> 'a t)
  val until : ('a -> bool) -> ('a -> 'a t) -> ('a -> 'a t)
end

module IterateDist : Iterate with type 'a t = 'a dist = struct
  type 'a t = 'a dist

  let rec iter n f x =
    if n <= 0 then certainly x
    else bind (f x) (iter (n - 1) f)

  let rec while_ p f x =
    if p x then bind (f x) (while_ p f)
    else certainly x

  let rec until p f x =
    if p x then certainly x
    else bind (f x) (until p f)
end

module IterateIO : Iterate with type 'a t = 'a random = struct
  type 'a t = 'a random

  let rec iter n f x =
    if n <= 0 then fun () -> x
    else
      let next = f x in
      fun () -> iter (n - 1) f (next ()) ()

  let rec while_ p f x =
    let next = f x in
    fun () ->
      let l = next () in
      if p l then while_ p f l ()
      else l

  let rec until p f x =
    let next = f x in
    fun () ->
      let l = next () in
      if p l then l
      else until p f l ()
end

(* 6 TRACING *)

(* Defines a trace as a list of 'a. *)
type 'a trace = 'a list

(* Defines a space as a distribution of traces. *)
type 'a space = 'a trace dist

(* Walk is a function from 'a to an 'a list. *)
type 'a walk = 'a -> 'a trace

(* Expand is from 'a to a trace distribution. *)
type 'a expand = 'a -> 'a space

(* Composes transitions with a space. *)
let ( >>: ) f g x =
  let D ds = g x in
  let D d = f (List.hd ds) in
  D (List.map (fun (y, p) -> (y :: ds, p)) d)

(* Bounded version of iterate for n steps. *)
let rec walk n f x =
  if n <= 0 then []
  else x :: walk (n - 1) f (f x)

(* Returns intermediate distributions for each step. *)
let rec ( *.. ) n t x =
  if n <= 0 then certainly [x]
  else
    let D d = t x in
    D (List.concat (List.map (fun (y, p) -> let D d' = ( *.. ) (n - 1) t y in List.map (fun (ys, q) -> (x :: ys, p *. q)) d') d))

(* Random tracing types. *)
type 'a rtrace = 'a trace random
type 'a rspace = 'a space random
type 'a rwalk = 'a -> 'a rtrace
type 'a rexpand = 'a -> 'a rspace

(* Composes the result of a random change with a random walk. *)
let composel_r f g x =
  let rs = g x in
  let r = List.hd rs in
  let s = f r in
  s :: rs

(* Recursively collects values by picking from a distribution each step. *)
let rec rwalk n t x =
  if n <= 0 then fun () -> [x]
  else
    let r = t x in
    let rs = rwalk (n - 1) t x in
    composel_r r rs

(* Transposes a list of lists, converting rows to columns and vice versa. *)
let rec transpose = function
  | [] | [] :: _ -> []
  | xss ->
      let heads = List.map List.hd xss in
      let tails = List.map List.tl xss in
      heads :: transpose tails

(* Merges a list of random traces into a list of distributions, one per position. *)
let merge_traces (trs : 'a rtrace list) : ('a dist list) random =
  fun () ->
    let trace_lists = List.map (fun tr -> tr ()) trs in
    let rows = transpose trace_lists in
    List.map (fun row -> norm (uniform row)) rows

(* SECTION 5 COT'D *)
    
(* Sim class defining ~. *)
module type Sim = sig
  type 'a t
  val sim : int -> ('a -> 'a t) -> 'a rtrans
  val sim_trace : (int * int) -> ('a -> 'a t) -> 'a rexpand
  val sim_iter : (int * int) -> ('a -> 'a t) -> 'a rtrans
end

module SimDist : Sim with type 'a t = 'a dist = struct
  type 'a t = 'a dist

  let sim n f x = fun () -> IterateDist.iter n f x

  let sim_trace (n, m) f x =
    let rec sim_trace' n m f x =
      if m <= 0 then certainly [x]
      else
        let D d = f x in
        (* really not proud of this - TODO *)
        let d' = sim_trace' n (m - 1) f in
        D (List.concat (List.map (fun (y, p) -> List.map (fun (ys, q) -> ((y :: ys), p *. q)) (let D d'' = d' y in d'')) d))
    in
    fun () -> sim_trace' n m f x

  let sim_iter (n, m) f x =
    let rec sim_iter' n m f x =
      if m <= 0 then certainly x
      else bind (f x) (sim_iter' n (m - 1) f)
    in
    fun () -> sim_iter' n m f x
end
