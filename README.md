# lenses-intro

## Introduction to lenses in Haskell - examples of usage.

### Command line

To compile and execute run:
```
$ ghc build
$ ghc exec lenses-intro-exe
```

or in one-liner:
```
$ ghc build && ghc exec lenses-intro-exe
```

### REPL

To use in GHC REPL run:
```
$ stack ghci --only-main
λ main
```
The option `--only-main` imports only main module.
You can skip it, but in that case when you try executing `run` function it will be ambiguous.

You can also run separate examples in `ghci` by:
```
λ Example1.run
...
λ Example2.run
... etc.
```

If you want to work in a scope of a specific module you can use for example:
```
λ :m +Example1
λ run
```
or simply:
```
λ import Example1
λ run
```

You can also "unimport" a specific module by:
```
λ :m -Example1
```

You can inspect your code in for example in following ways:
```
λ :t Point
Point :: a -> a -> Point a
λ :i Point
data Point a = Point {_x :: a, _y :: a}
instance Show a => Show (Point a)
λ :k Point
Point :: * -> *
λ :t x
x :: Functor f => (a -> f a) -> Point a -> f (Point a)
λ :i x
x :: Control.Lens.Type.Lens' (Point a) a
```
Where `:t` stands for type, `:i` for information and `:k` for kind.

You can reload the code after changes with `:r` and quit with `:q`.