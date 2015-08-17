---
layout: post
title: Implement semantic editor combinators
tags: haskell, lens
---

# Intro

In [Semantic editor combinators](http://conal.net/blog/posts/semantic-editor-combinators),
Conal has shown us a simple but powerful trick of walking into a portion of data
and then modifying it.

When I saw this simple idea, I persuaded myself to take it for grainted and tried it out.
Even though it works well in practice, I\'d like to explain to myself what\'s happening,
which is the motivation of this article.

I\'ll implement `first`, `second`, `result` and other combinators step by step,
and come up with the way to walking into user defined `data`, which is easy
but not presented in Conal\'s post.

# Editing a pair

First we need to think about what the type would be for `first` and `second`:

* When we reach for the portion of data, we need to modify it,
so we need a function `f :: a -> b`

* We also need a pair of data `(x,y) :: (c,d)` for this editor\'s input.

* For `first`, we want type `c = a`, and the return value should be of type `(b,d)`

* For `second`, we want type `d = a`, and the return value should be of type `(c,b)`

Put them together, we end up with type signature for `first` and `second`:

```haskell
first  :: (a -> b) -> (a,d) -> (b,d)
second :: (a -> b) -> (c,a) -> (c,b)
```

Introduce `f` and `(x,y)` for two arguments, and the implementation is straightforward:

```haskell
first  :: (a -> b) -> (a,d) -> (b,d)
first  f (x,y) = (f x, y)

second :: (a -> b) -> (c,a) -> (c,b)
second f (x,y) = (x, f y)
```

Let\'s bring up GHCi for some tests:

```haskell
λ> first (const 1) ("foo","bar") -- (1, "bar")
(1,"bar")
λ> second (* 20) (Just 1, 1) -- (Just 1, 20)
(Just 1,20)
```

# Editing a function

There\'s two way of editing a function:
we can either modify the return value (the `result` combinator),
or modify the argument (the `argument` combinator).

## the `result` combinator

Let\'s think about the type of `result`:

* How to modify a function is given by `f :: a -> b`
* The function we want to edit is `g :: c -> d`
* We want to modify on the return value of `g`, therefore `d = a`.
* Then the resulting function should be of type `c -> b`

```haskell
result :: (a -> b) -> (c -> a) -> (c -> b)
```

We know that `->` is right-associative,
(e.g. `p -> q -> r` and `p -> (q -> r)` are equivalent)
so we can also think `result` as a function that accepts 3 arguments,
and its type signature becomes:

```haskell
result :: (a -> b) -> (c -> a) -> c -> b
```

Introduce `f` `g` `x` for these 3 arguments, and we want a value of type `b`.

```haskell
result :: (a -> b) -> (c -> a) -> c -> b
result f g x = undefined
```

* To produce a value of `b`, we might need `f :: a -> b`.
* To produce a value of `a`, we might need `g :: c -> a`.
* `x` is of type `c`

Chaining these facts together, we yield the implementation of `result`:

```haskell
result :: (a -> b) -> (c -> a) -> c -> b
result f g x = f (g x)
```

This is exactly the definition of [`(.)`](http://hackage.haskell.org/package/base-4.6.0.1/docs/src/GHC-Base.html#.), function composition.

So we end up with:

```haskell
result :: (a -> b) -> (c -> a) -> c -> b
result = (.)
```

Test it:

```haskell
λ> result (+2) (*5) 1 -- 1 * 5 + 2 = 7
7
```

## the `argument` combinator

Use the same approach, let's implement `argument` combinator:

* `f :: a -> b` to modify the argument
* `g :: c -> d` the function to be modified
* `b = c` so that the type of `f` and `g` matches.
* the resulting function should be of type `a -> d`.

Write down the type signature, introduce `f`, `g`, `x`,
the goal is to produce a value of type `d`

```haskell
argument :: (a -> b) -> (b -> d) -> a -> d
argument f g x = undefined
```

* To produce a value of `d`, we might need `g :: b -> d`.
* To produce a value of `b`, we might need `f :: a -> b`.
* `x` is of type `a`

Therefore the definition of `argument` is:

```haskell
argument :: (a -> b) -> (b -> d) -> a -> d
argument f g x = g (f x)
```

To simplify this, we do some transformations:

```
  g (f x)
> (g . f) x           -- definition of (.)
> (.) g f x           -- infix operator
> ((.) g f) x         -- property of curried function
> ((flip (.)) f g) x  -- flip
> flip (.) f g x      -- rule of function application
```

Finally:

```haskell
argument :: (a -> b) -> (b -> d) -> a -> d
-- point-free style of: argument f g x = flip (.) f g x
argument = flip (.)
```

Test it:

```haskell
λ> argument show (\x -> x ++ "!") 2048 -- 2048!
"2048!"
```

# Walking into data

For now we have `first` and `second` to work with pairs,
and `argument` and `result` with functions,
but sometimes we want to go deeper inside data, say:

* Goal 1: we want `2` to be `5` in `(7,(2,1))`
* Goal 2: we want `a+b` to be `(a+b)*2` in `\a -> (a+1 ,\b -> (a+b, 1))`

Here I try to give some explanation of how to do this.

## Goal 1

To modify `d` in `(a,(c,d))`, that means we need to first focus on `(c,d)`,
this can be achieved by `second`,
let's pass a const function `const "Focus"` to `second`, so we know
we are focusing on which portion of the data:

```haskell
λ> let d1 = (7,(2,1))
λ> let toFocus = const "Focus"
λ> second toFocus d1
(7,"Focus")
```

We now know that missing `(2,1)` is passed to `toFocus`,
we can use a lambda expression to capture it:

```haskell
λ> second (\x -> x) d1
(7,(2,1))
```

Let\'s do something more interesting:

```haskell
λ> second (\x -> let (a,b) = x in (a+1,b,b)) d1
(7,(3,1,1))
```

The observation is: inside the body of lambda expression `\x -> ...`,
we can do something to `(2,1)`. That means we can apply semantic editor combinators on it!

Now assume we are dealing with `(2,1)`, to reach the goal, we apply `const 5` to its `fst` part:

```haskell
λ> first (const 5) (2,1)
(5,1)
```

Now replace the body of `\x -> ...` with what we want:

```haskell
λ> second (\x -> first (const 5) x) d1
(7,(5,1))
```

Done!

## Some intuition

Let\'s not hurry to Goal 2, I'll show you some transformations:

```
  second (\x -> first (const 5) x) d1
> second (first (const 5)) d1         -- eta reduction / point-free
> (second (first (const 5))) d1       -- rule of function application
> ((second . first) (const 5)) d1     -- definition of (.)
```

I don't simplify it to `(second . first) (const 5) d1` on purpose,
and I'm going to show you why.

It might not be straightforward at first glance,
since function composition is usually "right to left",
that is, `(g . f)` means "first apply `f`, on its result, apply `g`.
But here, `second . first` can be read as "walk to the `snd` part, and then `fst` part of it".

My explanation is: **the function does compose from right to left, but what's "carrying"
with the composition is not the data**.

The observation is: no matter how many functions are composed together,
the resulting function just takes one argument:

```haskell
λ> let u = undefined
λ> :t u . u
u . u :: a -> c
λ> :t u . u . u . u
u . u . u . u :: a -> c
λ> :t u . u . u . u . u
u . u . u . u . u :: a -> c
```

So what is the argument taken by this resulting function?
In our Goal 1, it is `const 5`, the "how we modify it" part.
So despite that `(second . first) (const 5) d1` is simple,
to understand it, what you really need would be:
`((second . first) (const 5)) d1`

The observation is: **functions are composed from right to left
and the mutator is buried more deeply as the composition goes**.

To see this, let\'s just pick up combinators randomly
to form a chain of function composition, and check its type:

```haskell
λ> :t first (const 5)
first (const 5) :: Num b => (a, d) -> (b, d)
λ> :t (second . first) (const 5)
(second . first) (const 5) :: Num b => (c, (a, d)) -> (c, (b, d))
λ> :t (result . second . first) (const 5)
(result . second . first) (const 5)
  :: Num b => (c -> (c1, (a, d))) -> c -> (c1, (b, d))
λ> :t (second . result . second . first) (const 5)
(second . result . second . first) (const 5)
  :: Num b => (c, c1 -> (c2, (a, d))) -> (c, c1 -> (c2, (b, d)))
λ> :t (first . second . result . second . first) (const 5)
(first . second . result . second . first) (const 5)
  :: Num b =>
     ((c, c1 -> (c2, (a, d1))), d) -> ((c, c1 -> (c2, (b, d1))), d)
```

Reading the composition from right to left, **we are constructing / definiting
the "shape" of the data this combinator would like to work on**. In contrast,
reading from left to right, **we are deconstructing the data
until we reach for the position that we want to modify**.
These two views does not conflict with each other.

Therefore, IMHO, when we are thinking about how to walking into the interesting portion,
it\'s helpful thinking about build up combinator from left to right.
But to answer the question about why, we should instead read from right to left.

In addition, here is another interesting observation:

```
    (<comb1> . <comb2>) (<comb3> . <mutator>) d1
<=> (<comb1> . <comb2> . <comb3>) <mutator> d1
```

The shape of semantic editor combinators are always:
`{comb} {mut} {dat}`

So if we see a combinator appear as the head of the `(.)` chain in `{mut}`,
we can move it to the tail of the `(.)` chain in `{comb}`,
and the other way around also holds true.

## Goal 2

Recall Goal 2: we want `a+b` to be `(a+b)*2` in `\a -> (a+1, \b -> (a+b, 1))`

Based on the previous understand, we need to:

* walk into `(a+1, \b -> (a+b, 1))`, using `result`
* walk into `\b -> (a+b, 1)`, using `second`
* walk into `(a+b, 1)`, using `result`
* walk into `(a+b)`, using `first`
* apply `(* 2)`

Try it out:

```haskell
λ> let d2 = \a -> (a+1, \b -> (a+b, 1))
λ> let m2 = (result . second . result . first) (*2) d2
λ> (snd (d2 10)) 20
(30,1)
λ> (snd (m2 10)) 20
(60,1)
λ> fst ((snd (d2 10)) 20)
30
λ> fst ((snd (m2 10)) 20)
60
λ> (fst . ($ 20) . snd . ($ 10)) d2
30
λ> (fst . ($ 20) . snd . ($ 10)) m2
60
```

Last two programs are just rewriting the two programs right before it,
Do you notice the symmetry between `fst . ($ 20) . snd . ($ 10)`
and `result . second . result . first`?

I believe this is not an accident, but to explain it might beyond my reach,
this is what I have for now:

With the help of [Lambdabot](http://www.haskell.org/haskellwiki/Lambdabot):

```
   (fst . ($ 20) . snd . ($ 10)) (result . second . result . first) (*2) d2
>  fst (snd (result (second (result (first 10)))) 20) (2 *) d2
```

I guess when `fst` and `first` meet together, or when `($ 10)` and `result` meet together, etc.
they will be somehow "cancelled". If someone knows how to finish this explanation,
please let me know.

# Play with lists

Conal has shown us `element`, which walks into every element of a list.

It's easy to work out its type signature:

* how to modify: `f :: a -> b`
* data to be modified: `[a]`
* resulting data: `[b]`

and this is exactly `map`.

```haskell
element :: (a -> b) -> [a] -> [b]
element = map
```

Here I show you 3 extra combinators:

*inHead* modifies the head of a list:

* how to modify: `f :: a -> a` (list can only contain elements of same types)
* data to be modified: `[a]`
* resulting data: `[a]`

```haskell
inHead :: (a -> a) -> [a] -> [a]
inHead f [] = []
inHead f (x:xs) = f x : xs
```

*inTail* modifies the tail of a list:

* how to modify: `f :: [a] -> [a]`
* data to be modified: `[a]`
* resulting data: `[a]`

```haskell
inTail :: ([a] -> [a]) -> [a] -> [a]
inTail f [] = []
inTail f (x:xs) = x : f xs
```

*inPos* modifies the element at a given position:

* we first need the position: `Int`
* how to modify: `f :: a -> a`
* data to be modified: `[a]`
* resulting data: `[a]`

```haskell
inPos :: Int -> (a -> a) -> [a] -> [a]
inPos n f xs
      | null xs    = xs
      | n < 0      = xs
      | n == 0     = let (y:ys) = xs
                     in f y : ys
      | otherwise  = let (y:ys) = xs
                     in y : inPos (n - 1) f ys
```

*inSome* is like `inPos`, but modifies multiple positions:

* we first need the indices: `[Int]`
* how to modify: `f :: a -> a`
* data to be modified: `[a]`
* resulting data: `[a]`

```haskell
inSome :: [Int] -> (a -> a) -> [a] -> [a]
inSome ixs f xs = foldl (\acc i -> inPos i f acc) xs ixs
```

Some examples:

```haskell
λ> let d1 = ("foo",[1..10])
λ> (second . element) (+ 1) d1
("foo",[2,3,4,5,6,7,8,9,10,11])
λ> (first . element) ord d1
([102,111,111],[1,2,3,4,5,6,7,8,9,10])
λ> (second . inTail) (take 2) d1
("foo",[1,2,3])
λ> (second . inHead) (* 200) d1
("foo",[200,2,3,4,5,6,7,8,9,10])
λ> (second . inPos 3) (* 200) d1
("foo",[1,2,3,800,5,6,7,8,9,10])
λ> let d2 = replicate 3 (replicate 4 0)
λ> (inPos 1 . inPos 2) (const 255) d2
[[0,0,0,0],[0,0,255,0],[0,0,0,0]]
```

# Walking into user defined data

Suppose user has defined a binary tree:

```haskell
data BinTree a = Node a
               | Tree (BinTree a) (BinTree a) a
                 deriving (Show, Eq, Functor)
```

To write a combinator is easy, just follow the pattern
`<combinator> <mutator> <data> = <modified-data>`:

```haskell
treeV :: (a -> a) -> BinTree a -> BinTree a
treeV f (Node x) = Node (f x)
treeV f (Tree l r x) = Tree l r (f x)

treeElement :: (a -> b) -> BinTree a -> BinTree b
treeElement = fmap

treeLeft :: (BinTree a -> BinTree a) -> BinTree a -> BinTree a
treeLeft f (Tree l r e) = Tree (f l) r e
treeLeft f r = r

treeRight :: (BinTree a -> BinTree a) -> BinTree a -> BinTree a
treeRight f (Tree l r e) = Tree l (f r) e
treeRight f r = r

treeNonLeaf :: (a -> a) -> BinTree a -> BinTree a
treeNonLeaf f (Tree l r e) = Tree (treeNonLeaf f l) (treeNonLeaf f r) (f e)
treeNonLeaf f r = r

treeLeaf :: (a -> a) -> BinTree a -> BinTree a
treeLeaf f (Node x) = Node (f x)
treeLeaf f (Tree l r x) = Tree (treeLeaf f l) (treeLeaf f r) x
```

Here `treeElement` walks into the data part of each node to do the modification.
Since `treeElement = fmap`, we can generalize `element = fmap`, to make it work for
not only lists and `BinTree`s, but also any other Functors.

`treeLeft` and `treeRight` walks into left subtree and right subtree, respectively.
But to target at the value of a `BinTree`, we need `treeV`, which takes care of walking
into the data field of a tree.

`treeNonLeaf` and `treeLeaf` are just like `treeElement` but works merely on
non-leaf nodes and leaf nodes, respectively.

See them in action:

```haskell
λ> let d1 = Tree (Tree (Node (1,1)) (Node (1,2)) (2,2)) (Node (3,4)) (5,6)
λ> (treeElement . first) even d1 -- test for each node, if the `fst` part is an even?
Tree (Tree (Node (False,1)) (Node (False,2)) (True,2)) (Node (False,4)) (False,6)
λ> (treeLeft . treeV . first) (* 4) d1 -- walk into left subtree, multiple `fst` part of its value by 4
Tree (Tree (Node (1,1)) (Node (1,2)) (2,2)) (Node (3,4)) (5,6)
λ> (treeLeft . treeRight . treeV . first) (* 4) d1 -- walk into the right subtree of the left subtree, ..
Tree (Tree (Node (1,1)) (Node (4,2)) (2,2)) (Node (3,4)) (5,6)
λ> (treeNonLeaf . first) (const 100) d1 -- for each value of the `fst` part of each non-leaf node
Tree (Tree (Node (1,1)) (Node (1,2)) (100,2)) (Node (3,4)) (100,6)
λ> (treeLeaf . second) (const 100) d1 -- for each value of the `snd` part of each leaf node
Tree (Tree (Node (1,100)) (Node (1,100)) (2,2)) (Node (3,100)) (5,6)
```

# Combining semantic editor combinators

The semantic editor combinators can also be chained together.

Suppose we have some combinators:

```haskell
λ> let c13 = (inPos 1) (const 3)
λ> let c05 = (inPos 0) (const 5)
λ> let c06 = (inPos 0) (const 6)
λ> let cf1 = first (const 1)
```

For a given list, we want to apply `c13` and `c05`:

```haskell
λ> let d = replicate 5 0
λ> c06 (c13 d)
[6,3,0,0,0]
λ> (c06 . c13) d
[6,3,0,0,0]
```

So we can use function composition to compose semantic editors:

```haskell
λ> let d2 = (10,replicate 5 0)
λ> (cf1 . (second c13)) d2
(1,[0,3,0,0,0])
λ> (cf1 . (second c13) . (second c05)) d2
(1,[5,3,0,0,0])
λ> (cf1 . (second (c13 . c05))) d2
(1,[5,3,0,0,0])
```

The last example says
"apply `1`, (back to the top level and then) walk into the `snd` part, apply `c05` and then `c13`".
I think this "distributive property" will make things extremely flexible.

Since the function composition happens from right to left,
if some combinators are trying to modify the same field, the "leftmost" one take effect:

```haskell
λ> (c05 . c06) [0,1]
[5,1]
λ> (c06 . c05) [0,1]
[6,1]
```
