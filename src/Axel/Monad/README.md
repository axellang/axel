**NOTE** These files all use the `UndecidableInstances` extension because our monad instances could lead to an infinite resolution loop. So, we're following `mtl`'s lead and biting the undecidable instances bullet.

See [Lifts for free: making mtl typeclasses derivable](https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/).
