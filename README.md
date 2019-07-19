# F-Algebra Data Combinator Generator

Generate an f-algebra combinator from any data type.

https://hackage.haskell.org/package/f-algebra-gen-0.1.0.0

## Description

This library provides a function to generate a special f-algebra combinator from any data
type (GADTs are not currently supported).

This was inspired by the recursion-schemes library where they have a function to
automagically generate a base functor. Although, this new base functor data type
has custom constructors and to define the \*-morphism algebras turns into
boring pattern matching.

So, this library provides a function called `makeCombinator` that produces a
nice combinator to deal with data types as they were defined in terms of Pairs
(`(,)`) and Sums (`Either`). With this nice combinator we are able to view a
data type as its equivalent categorical isomorphism and manipulate it with an
interface similar as the `either` function provided from `base`.

## Example

To create this special combinator you just need to call `makeCombinator ''<data
type name>` as in the example below:

```Haskell

-- List type
data List a = Nil | List a (List a)

makeBaseFunctor ''List

makeCombinator ''ListF
```

This example will generate the following code:

```Haskell
makeCombinator ''ListF
  ======>
    listf f_acw7 f_acw8 Nil = f_acw7 ()
    listf f_acw7 f_acw8 (Cons a_acw9 a_acwa) = f_acw8 (a_acw9, a_acwa)
```

As you can see it's pretty close as to have the type defined as the set of
sums and pairs `data List a = Either () (a, List a)`, which we could then use
`either` function as well as other convinent `(,)` combinators.

An **important** note is that the generated function has always the same name as
the data type but in low characters **and** the order of the functions to be
applied to the type constructors it's the same order which they were declared.

A simple example on how we can beneficiate from using this special combinator
when defining catamorphisms using recursion-schemes:

- Without the combinator:
  ```Haskell
  length :: [a] -> Int
  length = cata gene
    where
      gene Nil = 0
      gene (Cons a x) = x + 1
  ```

- With the combinator:
  ```Haskell
  makeCombinator'' ListF

  length :: [a] -> Int
  length = cata (listf (const 0) (succ . snd))
  ```

I recognize that for such a simple data type and catamorphism it's hard to see
any gain in readability/implementation. But with this special combinator it's a
lot easier to go from paper to code as it's almost a direct translation.

There's a fully working example in the `examples` folder that uses the
recursion-schemes library as well as a nice small program calculus (AoP
inspired) combinators library to show how simple and straightforward it is to
use it with this new combinator.
