## PFPCaml

A port of the library described in [this 2006 paper](https://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf) from Haskell to OCaml. This was ill-advised, because I didn't know Haskell or OCaml. I thought this would be a good opportunity to get comfortable with both. Also, the actual library has got some pretty interesting concepts.

I've translated the core of the module, [Probability.hs](https://github.com/lewis-carson/PFPCaml/blob/main/reference/Probability.hs) (from the original 2006 version) to [OCaml](https://github.com/lewis-carson/PFPCaml/blob/main/lib/probability.ml).

A lot of the time, this felt like trying to fit a round peg into a square hole:

- Type Definitions:
   - Haskell uses `newtype` for `Probability`, while OCaml uses `type`.
   - Haskell defines `Dist` as a `newtype`, whereas OCaml uses a regular type definition.

- Monad and Functor Instances:
   - Haskell defines `Monad` and `MonadPlus` instances for `Dist`.
   - OCaml uses functions like `bind` and `mplus` to achieve similar functionality.

- Random Values:
   - Haskell uses the `IO` monad for random values (`R` type).
   - OCaml uses a function type `unit -> 'a` for random values (`random` type).

- Pretty Printing:
   - Haskell uses the `Show` typeclass for pretty printing.
   - OCaml uses functions like `show_dist` for pretty printing.

- Expected Value Calculation:
   - Haskell uses the `Expected` typeclass.
   - OCaml uses a module type `Expected` and functors to achieve similar functionality.

- Iteration and Simulation:
   - Haskell uses the `Iterate` and `Sim` typeclasses.
   - OCaml uses modules and functors to define similar behavior.

- Conditional Probability:
   - Haskell uses the `|||` operator for filtering distributions.
   - OCaml uses the `|||` operator for similar functionality but implemented differently.

- Randomized Generators:
   - Haskell defines `RChange` and `RTrans` types for random changes and transitions.
   - OCaml defines similar types but uses different naming conventions and implementations.
