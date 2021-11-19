<div align="center">

# Refined Simple

![cabal](https://github.com/tbidne/refined-simple/workflows/cabal/badge.svg?branch=main)
![stack](https://github.com/tbidne/refined-simple/workflows/stack/badge.svg?branch=main)
![nix](https://github.com/tbidne/refined-simple/workflows/nix/badge.svg?branch=main)
![haddock](https://github.com/tbidne/refined-simple/workflows/haddock/badge.svg?branch=main)
![style](https://github.com/tbidne/refined-simple/workflows/style/badge.svg?branch=main)

</div>

### Table of Contents
- [Introduction](#introduction)
- [Background](#background)
- [Solutions](#solutions)
  - [Smart Constructors](#smart-constructors)
  - [Refined Types](#refined-types)
- [Summary](#summary)
- [Acknowledgements](#acknowledgements)

# Introduction

`refined-simple` provides a simple implementation of refined types with a small dependency footprint. Its main inspirations are [`LiquidHaskell`](https://ucsd-progsys.github.io/liquidhaskell/), [`refined`](https://hackage.haskell.org/package/refined) and [`facts`](https://hackage.haskell.org/package/facts).

# Background

Partial functions like

```haskell
div :: Integral a => a -> a -> a
```

are a scourge. Many a sleepless night was had debugging programming errors that occurred due to the errant usage of a partial function. The problem lies with its type signature. It is dishonest. `div` promises that we can pass any two integrals and receive an integral, but we know this is a lie because division by zero is undefined (and results in a runtime error).

One may wonder how best to handle partial functions. The simplest way to fix its type signature is to _widen the codomain_. That is, instead of the `div` above, we have

```haskell
div' :: Integral a => a -> a -> Maybe a
div' x 0 = Nothing
div' x d = Just (x `div` d)
```

With this method, we can map our "bad inputs" (i.e. `0` divisor) to `Nothing`. This is better than before because at least the type signature is honest, but this can easily lead to a proliferation of `Maybe`s, and it is especially frustrating when we _know_ that a call to `div` should not fail.

Rather than widening the codomain, a better strategy is _retricting the domain_:

```haskell
div' :: Integral a => a -> NonZero a -> a
```

That is, we force our consumers to create some `NonZero` type that serves as a "proof" that the underlying value is non-zero.

# Solutions

## Smart Constructors

One way to implement this pattern is known as the "smart-constructor" approach.

```haskell
-- Do not export MkNonZero!
data NonZero a = MkNonZero { unNonZero :: a }

mkNonZero :: (Num a, Ord a) => a -> Maybe (NonZero a)
mkNonZero (MkNonZero n)
  | n /= 0 = Just n
  | otherwise = Nothing

-- This is total, and no Maybes!
div' :: Integral a => a -> NonZero a -> a
div' n d = n `div` unNonZero d
```

A "smart-constructor" is simply a newtype wrapper that hides the underlying data constructor and instead provides a "smart" function (e.g. `mkNonZero`) that checks that the desired invariant holds. This way if we actually have our hands on a `NonZero`, we know that it is in fact non-zero.

This approach is simple and works well when we have just a few invariants we want to enforce. The main disadvantages are:

1. Does not compose (need totally disparate types for each invariant).
2. Checks must be done at runtime.

## Refined Types

Refined types are similar, though instead of a newtype for each invariant, we attach a phantom type representing a predicate:

```haskell
type Refined :: [Type] -> Type -> Type
newtype Refined ps a = UnsafeRefined { unrefine :: a }
```

We can then rewrite our `div` function as:

```haskell
div' :: Integral a => a -> Refined '[NonZero] a -> a
```

This has a number of advantages over smart constructors.

### Less Boilerplate

First, because we are using the same `Refined` type, we have the same API for all of our invariants. This cuts down on boilerplate like type declarations, helper functions, pattern synonyms, etc.

### Composition

Second, this is much more flexible. Not only can we compose predicates:

```haskell
-- e.g. 2
val :: Refined '[NonZero, Positive, LessThan 10, Even] Int
```

But with type families, we can also write functions that are _polymorphic_ in the predicate list:

```haskell
div' :: (Implies ps NonZero, Integral a) => a -> Refined ps a -> a

halved :: Integral a => a
halved x = div' x val
```

Thus we are free to demand only what we need, and our users can use our functions with maximum flexibility.

### Compile-Time Constants

Like with smart constructors, we can give users the ability to create refined types that might fail.

```haskell
refine :: Predicate p a => a -> Either RefineException (Refined '[p] a)
```

This makes sense for data that is truly unknown at compile-time, but it is dissatisfying for constants we _know_ cannot fail:

```haskell
-- This doesn't look good...
two :: Either RefineException (Refined '[NonZero] Int)
two = refine 2

-- And it gets worse!
divBy2 :: Int -> Either RefineException Int
divBy2 x = (x `div'`) <$> two
```

The fact that `divBy2` returns `Either` is absurd, because this is obviously a total function with no preconditions. You either have to live with a proliferation of `Maybe`/`Either` complicating your code, or you resort to partial functions like `error`, which are vulnerable to refactoring.

Fortunately, we can use `TemplateHaskell` to create refined types at _compile-time_ that cannot fail:

```haskell
refineTH :: forall p a. (Predicate p a, Lift a) => a -> Q (TExp (Refined '[p] a))

two :: Refined '[NonZero] Int
two = $$((refineTH 2))

divBy2 :: Int -> Int
divBy2 x = x `div'` two
```

# Summary

In general, we should write total functions, and the best way to turn a partial function into a total function is to restrict the "bad inputs" in the first place, rather than returning an error. Smart constructors are one way to achieve this, and it works well if the needed invariants are few.

On the other hand, refined types give us a solution that is:

* Lower boilerplate
* Compositional
* Safer

# Acknowledgements

`LiquidHaskell` was the main source of inspiration for refined types in general, while both `refined` and `facts` served as guides on how to achieve this in pure Haskell. The primary differences from the latter two libraries are:

* Refined
  * Only dependencies are `TemplateHaskell` and `Text`, giving `refined-simple` a significantly lighter dependency footprint.
  * Predicate composition is a type-level list (like `facts`), not a combination of logical operators. This makes composition less general than `refined`'s, but it is easier to work with.
* Facts
  * Fewer dependencies (though `facts` is also light).
  * Simpler API.
  * More built-in predicates.


In short: If you want a "batteries included" refined library with the highest ecosystem integration and most flexible composition (and don't mind some transitive dependencies you may not use e.g. `aeson`), check out `refined`. If instead you want a more compact design with a strong mathematical foundation, try `facts`. Finally, if you would ordinarily use smart constructors but are tired of some of the shortcomings, then perhaps give `refined-simple` a try.