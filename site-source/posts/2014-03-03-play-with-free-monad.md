---
layout: post
title: Play with free monad
tags: haskell, type-tetris, monad
---

This article is not a free monad tutorial
but some ideas about what free monad does from the prospective of a Haskell newbie.

Some days ago I came across
[Free monad](http://hackage.haskell.org/package/free-4.4/docs/Control-Monad-Free.html)
by reading
[this Stack Overflow question](http://stackoverflow.com/questions/13352205/what-are-free-monads).

I\'m not sure about what exactly it is,
although some intuitiions are gained through both reading the question and
answers and playing with the library.

In the rest of this article, I\'d like to share my experience about
how I explore functions listed
[here](http://hackage.haskell.org/package/free-4.4/docs/Control-Monad-Free.html)
and some intuitions gained from this process.
These intuitions are marked as **bold sentences**.

# Make some values of `Free f a`

Import related library, you shoule make sure you\'ve installed
[The free package](http://hackage.haskell.org/package/free-4.4).

```haskell
import Control.Monad
import Control.Monad.Free
```

This first thing is to make some values of type `Free f a`.

The easiest constructor is `Pure`.
In order to fit the type `Free f a`, I choose to use a list as `f` and `Int` as `a`,
because when I looked around the document, `f` is most likely to be an instance of `Functor`:

```haskell
v1 :: Free [] Int
v1 = Pure 10
```

The next thing is about how to use the other constructor: `Free`.
Its type signature is `Free :: f (Free f a) -> Free f a`.
It might not be that friendly, so I simplify it a little with `f = [], a = Int`.
Then it becomes: `Free :: [Free [] Int] -> Free [] Int`.
Go back to the original signature, we can say that `Free` constructor takes
some value of `Free f a` wrapped in another `f`, then produces `Free f a`.
By using `Free`, **the outside functor is somehow "ignored" from its type signature.**
Moreover, the part `f (Free f a)` suggests
**an exactly match of inside and outside functors.**

Now we\'ve already have a `v1 :: Free [] Int`, and by using `Free`,
we should end up with some values of type `Free [] a`:

```haskell
v2 :: Free [] Int
v2 = Free [v1]

v3 :: Free [] Int
v3 = Free [v2,v2,v1]
```

Call GHCi to see the values:

```haskell
λ> v1
Pure 10
λ> v2
Free [Pure 10]
λ> v3
Free [Free [Pure 10],Free [Pure 10],Pure 10]
```

# Free monad is a monad

Of course free monad should be a monad, let\'s check the document
to find some clues, in the instance list of `data Free f a`, we find this line:

```haskell
Functor f => Monad (Free f)
```

This is enough for us to write some functions
that can be used as the second argument of
our old friend `>>=`:

```haskell
foo1 :: Int -> Free [] Int
foo1 x = Free [Pure $ x*2, Pure $ x+20]

foo2 :: Int -> Free [] String
foo2 x = Free [Pure $ show x]

v4 :: Free [] String
v4 = v1 >>= foo1 >>= foo1 >>= foo1 >>= foo2
```

And the GHCi output is:

```haskell
λ> v4
Free [Free [Free [Free [Pure "80"]
                 ,Free [Pure "60"]]
           ,Free [Free [Pure "80"]
                 ,Free [Pure "60"]]]
     ,Free [Free [Free [Pure "120"]
                 ,Free [Pure "80"]]
           ,Free [Free [Pure "100"]
                 ,Free [Pure "70"]]]]
λ> :{
| let foo1 = \x -> [x*2,x+20]
|     foo2 = \x -> [show x]
| in [10] >>= foo1 >>= foo1 >>= foo1 >>= foo2
| :}
["80","60","80","60","120","80","100","70"]
```

You can observe some similiarity between list monad and `Free []` monad.
The intuition is **`Free []` monad seems to be a list monad but without `concat`**.

# `retract` and `liftF`

The document says "`retract` is the left inverse of `lift` and `liftF`".
So we explore `liftF` first.

`liftF` is "A version of lift that can be used with just a Functor for f."
I\'m not sure about this, but we can get started from another clue: the type signature
`liftF :: (Functor f, MonadFree f m) => f a -> m a`.

We choose to stick to our simplification: `f = []`, so we need to
find a suitable `m` that is an instance of `MonadFree [] m`.

And this one looks promising:

```haskell
Functor f => MonadFree f (Free f)
```

So we can let `m = Free f`, which gives us the simplified type signature:
`liftF :: [a] -> Free [] a`. We can guess that **`liftF` lifts functors
into free monads**. To confirm about this, let\'s try to lift some functors:

```haskell
v7 :: Free [] Int
v7 = liftF [1,2,3,4,8,8,8]

v8 :: Free Maybe String
v8 = liftF $ Just "Foo"

v9 :: Free ((->) Int) Int
v9 = liftF (+ 10)
```

I don\'t know if there is an easy way of observing `v9`,
for now we just print `v7` and `v8`:

```haskell
λ> v7
Free [Pure 1,Pure 2,Pure 3,Pure 4,Pure 8,Pure 8,Pure 8]
λ> v8
Free (Just (Pure "Foo"))
```

Now we know we can lift any functors, so **we can construct `Free f a`
not only from `Pure` and `Free`, but also directly from functors.**

Now think about simplified type signature of `retract :: Free [] a -> [a]`.
**it is the reverse of `liftF`**. Let\'s have a try in code:

```haskell
v10 :: [Int]
v10 = retract v3

v11 :: Maybe String
v11 = retract v8

v12 :: [String]
v12 = retract v4

v13 :: Int -> Int
v13 = retract v9
```

Call GHCi for results, now we can observe `v9` indirectly by using
function `v13`:

```haskell
λ> v10
[10,10,10]
λ> v11
Just "Foo"
λ> v12
["80","60","80","60","120","80","100","70"]
λ> map v13 [1..5]
[11,12,13,14,15]
````

Therefore  **`retract` converts a free monad back to a functor**.
(I suspect this conclusion is somehow wrong, but for now
I cannot tell what exactly is wrong.)

# `iter` and `iterM`

Use the same trick we\'ve played before, let `f = []`:

```haskell
iter  ::            ([  a] ->    a) -> Free [] a ->   a
iterM :: Monad m => ([m a] -> m  a) -> Free [] a -> m a
```

I guess: `iter` converts a function who does some sorts of reduction
on functors into another function who does the same thing but
on free monads, and `iterM` is the monadic version of `iter`.

Let\'s have a try:

```haskell
v14 :: Int
v14 = iter sum v3

foo3 :: [IO Int] -> IO Int
foo3 ms = sum `fmap` sequence ms

v15 :: IO Int
v15 = iterM foo3 v7
```

And look at GHCi:

```haskell
λ> v14
30
λ> v15
34
```

Maybe I can say that **`iter` and `iterM` lift reduction (for functors)
into free monads**

# `hoistFree`

The type signature contains `forall`, but I think it\'s fine
to just ignore that part. As we are just trying to get things work:

```haskell
hoistFree :: Functor g => (f a -> g a) -> Free f b -> Free g b
```

I guess **`hoistFree` lift a conversion between functors into
a conversion between free monads.**

I know list and `Maybe` are good friends:

```haskell
import Data.Maybe
v16 :: Maybe String
v16 = Just "Foo"

v17 :: [String]
v17 = retract $ hoistFree maybeToList $ liftF v16
```

Look at GHCi, we should observe the conversion
from `Maybe String` to `[String]`:

```haskell
λ> v16
Just "Foo"
λ> v17
["Foo"]
```

# `_Pure` and `_Free`

The type signatures of these two functions are really long,
but don\'t be afraid, we still have some clue:

* If you look at [`Choice`](http://hackage.haskell.org/package/profunctors-4.0.2/docs/Data-Profunctor.html#t:Choice), we find a simple instance: `Choice (->)`
* Pick up an applicative, I choose `m = []`
* Pick up a functor, I choose `f = Maybe`
* Ignore `forall` parts

We end up with:

```haskell
_Pure :: (            a -> [           a])
      -> ( Free Maybe a -> [Free Maybe a])
_Free :: ((Maybe (Free Maybe a)) -> [Maybe (Free Maybe a)])
      -> (       (Free Maybe a)  -> [       Free Maybe a ])
```

I really can\'t figure out what these functions do,
but I can still use it by feeding it with values of suitable type.

So for the following part of this section, I provide codes and outputs
without explanation.

```haskell
v18 :: [Free Maybe String]
v18 = _Pure (:[]) (liftF $ Just "Haskell")

v19 :: [String]
v19 = mapMaybe retract v18

-- f = Maybe
-- p = (->)
-- m = IO
-- _Free :: (Maybe (Free Maybe a) -> IO (Maybe (Free Maybe a)))
--       -> (       Free Maybe a  -> IO        (Free Maybe a) )
v20 :: IO (Free Maybe Int)
v20 = _Free return (liftF $ Just 123456)

v21 :: IO (Maybe Int)
v21 = retract `fmap` v20
```

GHCi:

```haskell
λ> v18
[Free (Just (Pure "Haskell"))]
λ> v19
["Haskell"]
λ> v20
Free (Just (Pure 123456))
λ> v21
Just 123456
```

# `wrap` method

This is a method defined for `MonadFree`s, the document says
**"Add a layer."**, let\'s see it in action:

```haskell
v22 :: Free Maybe Int
v22 = liftF $ Just 10

v23 :: Free Maybe Int
v23 = wrap (Just v22)

v24 :: Free Maybe Int
v24 = wrap (Just v23)
```

GHCi:

```haskell
λ> v22
Free (Just (Pure 10))
λ> v23
Free (Just (Free (Just (Pure 10))))
λ> v24
Free (Just (Free (Just (Free (Just (Pure 10))))))
```

# Call it a day

This is the first time I attempted to do a "type-tetris",
although I still don\'t know what is free monad, but I do get some
intuitions by using the functions provided. So let\'s call it a day.
