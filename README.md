[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/tZ2xolrg)
# Haskell: Infinite lists

<img alt="points bar" align="right" height="36" src="../../blob/badges/.github/badges/points-bar.svg" />

<details>
<summary>Guidelines</summary>

## Guidelines

When solving the homework, strive to create not just code that works, but code that is readable and concise.
Try to write small functions which perform just a single task, and then combine those smaller
pieces to create more complex functions.

Don’t repeat yourself: write one function for each logical task, and reuse functions as necessary.

Don't be afraid to introduce new functions where you see fit.

### Sources

Each task has corresponding source file in [src](src) directory where you should implement the solution.

### Building

All solutions should compile without warnings with following command:

```bash
stack build
```

### Testing

You can and should run automated tests before pushing solution to GitHub via

```bash
stack test --test-arguments "-p TaskX"
```

where `X` in `TaskX` should be number of corresponding Task to be tested.

So to run all test for the first task you should use following command:

```bash
stack test --test-arguments "-p Task1"
```

You can also run tests for all tasks with just

```bash
stack test
```

### Debugging

For debugging you should use GHCi via stack:

```bash
stack ghci
```

You can then load your solution for particular task using `:load TaskX` command.

Here is how to load Task1 in GHCi:

```bash
$ stack ghci
ghci> :load Task1
[1 of 1] Compiling Task1 ( .../src/Task1.hs, interpreted )
Ok, one module loaded.
```

> **Note:** if you updated solution, it can be quickly reloaded in the same GHCi session with `:reload` command
> ```bash
> ghci> :reload
> ```

</details>

## Preface

This assignment is all about exploiting Haskell's laziness by
creating and manipulating infinite sequences.

> It is recommended for tasks to be implemented in order.

## Task 1 (2 points)

There are countless ways to obtain infinite sequences in Haskell,
even just using standard `Prelude`.

For example, to get infinite list of natural numbers you can

- use built-in list generators
  ```haskell
  nats = [1..]
  ```
- use [iterate](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:iterate) from `Prelude`
  ```haskell
  nats = iterate succ 1
  ```
- use recursion with [map](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:map)
  ```haskell
  nats = 1 : map succ nats
  ```
- or more elaborate combination of built-in functions
  (in this case [zipWith](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:zipWith)
  and [repeat](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:repeat))
  ```haskell
  nats = 1 : zipWith (+) (repeat 1) nats
  ```

> [!TIP]
>
> Try to work out how and why these examples work by playing with
> these definitions in GHCi and examining documentation for used functions.

Yet, there is one more way to generate lists (both finite and infinite) using function 
[unfoldr](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:unfoldr) from `Data.List`.

```haskell
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
```

As written in documentation, it is a *dual* to
[foldr](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:foldr),
but instead of reducing list to a single value, `unfoldr` *builds* the list from a seed value
using provided function, which returns next value and new seed wrapped into `Maybe`
with `Nothing` denoting end of the list.

Your goal is to implement using `unfoldr` following infinite sequences:

- Natural numbers $`\mathbb{N} = \{1, 2, 3, ... \}`$ (excluding zero)
  ```haskell
  nats :: [Integer]
  ```
  **First 10 numbers:**
  ```haskell
  >>> take 10 nats
  [1,2,3,4,5,6,7,8,9,10]
  ```
- Fibonacci numbers (starting with zero)
  ```haskell
  fibs :: [Integer]
  ```
  **First 10 numbers:**
  ```haskell
  >>> take 10 fibs
  [0,1,1,2,3,5,8,13,21,34]
  ```
- Prime numbers using [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Incremental_sieve)
  ```haskell
  primes :: [Integer]
  ```
  **First 10 numbers:**
  ```haskell
  >>> take 10 primes
  [2,3,5,7,11,13,17,19,23,29]
  ```

> [!NOTE]
>
> Typically Sieve of Eratosthenes is used to produce primes up to a fixed limit.
> However, in Haskell we can easily extend this idea to generate infinite list of all
> prime numbers.
>
> Take a look at `sieve` function in [src/Task1.hs](src/Task1.hs) to get
> a clue for how to accomplish this (and then implement `primes` with `sieve`).

## Task 2 (4 points)

You may have noticed that we don't ever need to return `Nothing` in `unfoldr`
when defining infinite lists. So `Maybe` wrapper in step function of `unfoldr`
is redundant for our purposes.

We can make our intentions more explicit by defining a type that can only
represent infinite sequences:

```haskell
-- | Infinite stream of elements
data Stream a = Stream a (Stream a)
```

For such type `unfold` function could be simplified by removing `Maybe` from step function:

```haskell
unfold :: (b -> (a, b)) -> b -> Stream a
```

Your goal in this task is to implement the same infinite sequences as in the first task,
only using `Stream` instead of built-in list.

### Show

It might be tempting to add `deriving Show` to definition of `Stream`,
but it will not get us a usable result. Remember that `Stream` represents
*infinite* sequences, so the derived implementation of `Show` will attempt
to print all elements from this sequence which will never end.

Instead let's just show the first 10 or 20 elements, to get the general idea of what
given sequence is about.

For this you need to manually define an instance of `Show` for `Stream`.

### Conversion to/from list

Next you need to implement conversion to and from built-in lists.

For conversion *to* list we can use aptly named function [toList](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html#v:toList) from `Data.Foldable`. This means that you should implement an instance of `Foldable` for `Stream`.

The opposite conversion will have to be implemented as a separate function `fromList`:

```haskell
fromList :: a -> [a] -> Stream a
```

**Example:**

```haskell
>>> fromList 0 [1,2,3]
[1,2,3,0,0,0,0,0,0,0]
>>> fromList undefined [1..]
[1,2,3,4,5,6,7,8,9,10]
```

To convert from finite lists this function accepts additional argument --- element
which will be repeated infinitely after the list has ended.

### Unfolding streams

Next implement function `unfold`:

```haskell
unfold :: (b -> (a, b)) -> b -> Stream a
```

**Example:**

```haskell
>>> unfold (\x -> (x, x-1)) 5
[5,4,3,2,1,0,-1,-2,-3,-4]
>>> unfold (\x -> (abs x, x-1)) 5
[5,4,3,2,1,0,1,2,3,4]
```

Finally, using `unfold` implement following infinite sequences:

- Natural numbers $`\mathbb{N} = \{1, 2, 3, ... \}`$ (excluding zero)
  ```haskell
  nats :: [Integer]
  ```
- Fibonacci numbers (starting with zero)
  ```haskell
  fibs :: [Integer]
  ```
- Prime numbers using [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Incremental_sieve)
  (see note from the first task)
  ```haskell
  primes :: Stream Integer
  ```

> [!TIP]
>
> You might find it useful to implement an instance of
> [Functor](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Functor) for `Stream`
> as well as `Stream` analogs of other built-in functions, such as
> [iterate](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:iterate),
> [repeat](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:repeat),
> [filter](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:filter) etc.

## Task 3 (4 points)

In the last task you will again implement infinite sequences of natural numbers and Fibonacci
numbers, but in very unusual way --- using [generating functions](https://en.wikipedia.org/wiki/Generating_function).

> In mathematics, a generating function is a representation of an infinite sequence of numbers
> as the coefficients of a [formal power series](https://en.wikipedia.org/wiki/Formal_power_series).
> Generating functions are often expressed in
> [closed form](https://en.wikipedia.org/wiki/Closed-form_expression) (rather than as a series),
> by some expression involving operations on the formal series.
>
> -- [Generating function, Wikipedia](https://en.wikipedia.org/wiki/Generating_function)

For example, we can get infinite constant sequence $1, 1, 1, 1, ...$ as coefficients of geometric series:

```math
\frac{1}{1 - x} = \sum^{\infty}_{n = 0} x ^ n = 1 + 1 x + 1 x^2 + ...
```

Moreover, such generating functions exist for both natural numbers and Fibonacci numbers (sadly not for prime numbers).

### Idea

The general idea is to encode *generating functions* of the form

$$
a_0 + a_1 x + a_2 x^2 + ... + a_n x^n + ...
$$

as `Stream` of coefficients $[a_0, a_1, a_2, ..., a_n, ...]$ completely ignoring $x$,
since we are only interested in the sequence produced by coefficients, not the value of
generating function itself. With this representation we could define all necessary
numerical operations to write formula like $\frac{1}{1 - x}$ where $1$, $x$ and
the whole formula have underlying type `Stream Integer`, thus yielding desired sequence in the end.

### Series

In [src/Task3.hs](src/Task3.hs) you will find definition of wrapper `Series` around `Stream`:

```haskell
-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
newtype Series a = Series
  { coefficients :: Stream a }
```

Along with following examples of expected usage:

```haskell
>>> coefficients (x + x ^ 2 + x ^ 4)
[0,1,1,0,1,0,0,0,0,0]
>>> coefficients ((1 + x)^5)
[1,5,10,10,5,1,0,0,0,0]
>>> coefficients (42 :: Series Integer)
[42,0,0,0,0,0,0,0,0,0]
```

However, for these examples to work you will need to
make more preparations.

### $x$

To start off, as part of our [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for
describing power series, you need to implement function `x` which returns
`Series` corresponding to single $x$:

```haskell
x :: Num a => Series a
```

For that you should use following equality:

```math
x = 0 + 1 x + 0 x^2 + ...
```

Which corresponds to coefficients `[0,1,0,0,...]`.

### Num

Now you need to make `Series a` an instance of type class `Num` assuming
that `a` is also `Num`. For that you should implement following methods:

- `fromInteger` that converts `Integer` to `Series` using the following
  equality:
```math
n = n + 0 x + 0 x^2 + ...
```
- `negate` that negates all coefficients
- Addition `(+)` which for infinite power series is just a matter of adding
  corresponding coefficients:  
```math
\begin{aligned}
(a_0 + a_1 x + a_2 x^2 + ...) &+ (b_0 + b_1 x + b_2 x^2 + ...) \\
    &= (a_0 + b_0) + (a_1 + b_1) x + (a_2 + b_2) x^2 + ...
\end{aligned}
```
- Multiplication `(+)` which is slightly more involved than addition:  
  Suppose $A = a_0 + xA'$ and $B = b_0 + xB'$ are series that we want to multiply. Then
```math
\begin{aligned}
AB &= (a_0 + xA')B \\
   &= a_0 B + x A' B \\
   &= a_0 (b_0 + xB') + x A' B \\
   &= a_0 b_0 + x (a_0 B' + A' B)
\end{aligned}
```
- Lastly, there are functions `abs` and `signum`, which will not be really needed for
  this task. So you can implement them in any way you want, as long as it satisfies
  their [law](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:signum)
  as defined in documentation:
  ```haskell
  abs a * signum a == a
  ```

You will definitely find it helpful to also implement a utility operator for multiplying
whole power series by given number:

```haskell
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
```

**Example:**

```haskell
>>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
[0,2,2,0,2,0,0,0,0,0]
>>> coefficients (2 *: ((1 + x)^5))
[2,10,20,20,10,2,0,0,0,0]
```

> [!IMPORTANT]
> 
> After this, all the examples mentioned above should work,
> so you should try them out in GHCi and see that they all work as expected.

### Fractional

To be able to produce required generating functions like geometric series $\frac{1}{1 - x}$,
we should be able to divide `Series`. There are two built-in division functions
in Haskell: `div` from type class `Integral` and `(/)` from type class `Fractional`.

Although we will only work with evenly divisible coefficients, it is better to define
full `Fractional` instance for `Series a` assuming that `a` is also `Fractional`.

Type class `Fractional` has only two methods:

- `fromRational` which can be implemented similarly to `fromInteger`
- Division `(/)` which has the following formula (you can try to prove it yourself):  
  Suppose $A = a_0 + xA'$ and $B = b_0 + xB'$ are series that we want to divide. Then
```math
\frac{A}{B} = \frac{a_0}{b_0} + x (\frac{A' - \frac{a_0}{b_0}B'}{B})
```

### Generating functions

Finally, we have almost everything to use generating functions for obtaining
desired infinite sequences.

Ideally we would like to define function `ones` like this:

```haskell
ones :: Stream Integer
ones = coefficients (1 / (1 - x))
```

Which should produce infinite stream of ones because of generating function:

```math
\frac{1}{1 - x} = \sum^{\infty}_{n = 0} x ^ n = 1 + 1 x + 1 x^2 + ...
```

However, this definition of `ones` will not compile:

```
• No instance for (Fractional Integer) arising from a use of ‘/’
• In the first argument of ‘coefficients’, namely ‘(1 / (1 - x))’
  In the expression: coefficients (1 / (1 - x))
  In an equation for ‘ones’: ones = coefficients (1 / (1 - x)) [-Wdeferred-type-errors]
```

The problem is that `Integer` is not instance of `Fractional`, so we can't divide
coefficients using `(/)`. But there is a built-in type that will fit perfectly
for representing *rational* coefficients ---
[Data.Ratio](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Ratio.html#t:Ratio).
It represents rational numbers $\frac{a}{b}$ with numerator $a$ and denominator $b$
of some `Integral` type (which is perfect since `Integer` is `Integral`).

Here are some examples (operator `(%)` is constructor of `Ratio` with given numerator and denominator):

```haskell
>>> numerator (2 % 3)
2
>>> denominator (2 % 3)
2
>>> 2 % 3 + 4 % 3
2 % 1
```

The idea is to perform all computations using `Ratio Integer` and then
convert `Series (Ratio Integer)` to `Stream Integer` using function `gen` by simply ignoring
denominator, relying on the fact that all coefficients will be actual integer values
(i.e. with denominator equal to $1$).

This function `gen` is for you to implement:

```haskell
gen :: Series (Ratio Integer) -> Stream Integer
```

Then function `ones` can be implemented simply as

```haskell
ones :: Stream Integer
ones = gen (1 / (1 - x))
```

### Natural numbers

Implement function `nats` using following generating function for natural numbers

```math
\frac{1}{(1 - x)^2} = \sum^{\infty}_{n = 0} (n + 1) x ^ n = 1 + 2 x + 3 x^2 + ...
```

### Fibonacci numbers

Lastly, implement function `fibs` using following generating function for Fibonacci numbers

```math
\frac{x}{1 - x - x^2} = F_0 + F_1 x + F_2 x ^ 2 + ...
```

where $F_0 = 0$, $F_1 = 1$ and $F_i = F_{i - 1} + F_{i -2}$ for $i \geq 2$

You should first try to prove this equation yourself, but if you get stuck,
take a look at the sketch of the proof below.

<details>
<summary>Proof sketch</summary>

Suppose we already have generating function $F(x)$ for Fibonacci numbers:

$F(x) = F_0 + F_1 x + F_2 x^2 + ...$

Notice that $x + x F(x) + x^2 F(x) = F(x)$ by definition of Fibonacci coefficients $F_i$.
Now it is just a matter of rearranging the terms:

- $x = F(x) - x F(x) - x^2 F(x)$
- $F(x) = \frac{x}{1 - x - x^2}$

<div align=right>∎</div>

</details>



