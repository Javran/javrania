---
layout: post
title: Comonad, Zipper and Conway's Game of Life (Part 1)
tags: haskell, comonad, zipper, type-tetris
---

(Updated on Aug 22, 2014, better title, add link)

# Intro

Let\'s go beyond monad, today I want to show you
how to play with [comonad](http://hackage.haskell.org/package/comonad).
I don\'t understand the theory behind comonad,
but programming with it is quite simple. Basically,
I think **it\'s just an algebraic structure dealing with data context.**

In other words, if you find yourself dealing with some recursive data structure,
and in that data structure, the value of one place depends on the value of its neighborhoods
(some "data context", not sure if I\'m using the right terminology),
you probably want to take a look at comonad.

# Conway\'s Game of Life

To do something interesting with comonad, the first thing came to my mind was
[Game of Life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
It\'s a simple "game" that involves no players.
At the beginning, a world is represented as a 2D array of booleans.
Then this world evolves following some simple
[rules](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules).

Looking at these rules, you will find that the next state of a given cell in the world
is merely determined by the previous states of itself and its neighborhoods.
So I feel Game of Life would be a perfect example for comonad.

# Comonad

Recall what is a monad in Haskell code:

```haskell
class Functor m => Monad m where
    return :: a -> m a
    join   :: m (m a) -> m a
```

I lied here because to define a `Monad`, Haskell does not require you to
make it an instance of `Functor` at the first place.
But trust me
[this requirement will be forced in future](http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal).

In addition, I don\'t define a `(>>=)` but instead a `join`, since `f >>= m = join (fmap f m)`,
and `m` should also be an instance of `Functor`, this monad definition should be almost equivalent
to the one found in the base library.

Whenever we "co-" something, it means to filp every `->` into `<-`. So we know
what a comonad would look like:

```haskell
class Functor w => Comonad w where
    extract   :: w a -> a
    duplicate :: w a -> w (w a)

    extend    :: (w a -> b) -> w a -> w b
    (=>>)     :: w a -> (w a -> b) -> w b
```

Here you can see we flip the type signature for `return` and `join`,
we get `extract` and `duplicate` as results.
It\'s also handy to have `extend` and `(=>>)`, which can all be defined
in terms of `fmap` and `duplicate`, just like `(=<<)` and `(>>=)` functions for monad.
Let\'s try to figure out the implementation of `extend`.

The first step is to bind the arguments of `extend` to some name (here `f` and `w1`),
so we can list all the tools available:

```haskell
extract    :: forall a   . w a -> a
duplicate  :: forall a   . w a -> w (w a)
fmap       :: forall a b . (a -> b) -> w a -> w b
f          :: w a -> b
w1         :: w a
-- goal:
extend f w :: w b
```

It\'s tempting to try `f w1 :: b`, but it is a dead end.
Instead, because `f` "removes a layer of w", we can try to add another layer before using `f`:

```haskell
duplicate w1          :: w (w a)
fmap                  :: (a' -> b') -> w a' -> w b'
-- where a' = w a, b' = b
fmap f (duplicate w1) :: w b
```

Therefore:

```haskell
extend f w = fmap f (duplicate w)
w =>> f    = extend f w
```

or more point-freely:

```haskell
extend f = fmap f . duplicate
(=>>)    = flip extend
```

# Some understanding about comonad

In my opinion, when trying to understand monad,
it\'s more straightforward to begin with understanding `fmap`, `return` and `join`,
but in practice, `>>=` and `=<<` come in handy.
And the same thing happens to comonad: `extract` and `duplicate` are easy,
and `extend` and `=>>` come in handy.

Recall that in the intro section, I said comonad deals with data context.
More precisely, (IMHO) it\'s a type constructor that adds to a data structure
the ability of focusing on one particular value and the ability of moving focus.

* `extract` means to extract a value from the current focus,
this can be seen from its type signature `extract :: w a -> a`.
* `duplicate` might not be that straightforward to understand. It means to replace
every value in the data structure, with its corresponding context.

To make an analogy, assume you are inside of a big building,
there are maps indicating the spot you are currently standing.
If this building were a comonad, and the focus was on you. I can use `extract` to
directly find you. In addition, there are some places with maps. These maps
provides you with not only the whole structure of this building, but also the
place where every map itself is located. So if you are inside a building near
a map, you can take a picture on the map and send it to me, then I can locate you
using `extract`.

Now suppose there is a building without maps, what `duplicate` does is just like
drawing maps for each place inside the building and putting them to their right places.
So `w` is the building "comonad", `a` is a single place inside the building and
`duplicate :: w a -> w (w a)` draws maps and put them to their corresponding places.

And just like `(>>=)` in monad, `(=>>)` is more handy to use in comonad.
Looking at its signature `(=>>) :: (w a -> b) -> w a -> w b`,
the most interesting part is its first argument, a function of type `w a -> b`.
This argument is exactly where rules come into play:
"Given a data with focus (`w a`),
let\'s determine the next value of the current focus will be (`b`)."

I\'d like to call it a day here.
In the future posts, we\'ll see these functions in action.

# Other parts

* [Part 2](2014-08-22-comonad-zipper-and-conways-game-of-life.html)
