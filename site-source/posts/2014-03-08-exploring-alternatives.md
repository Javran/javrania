---
layout: post
title: Exploring alternatives
tags: type-tetris, haskell
---

Today I\'m going to play with
[Alternatives](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Applicative.html#t:Alternative).

(spoiler: I failed to understand what `some` and `many` do,
and I guess that would be an essential part of getting `Alternative`.
So there\'s not much thing useful in this article.)

Just as my other type-tetris attempts, this is not a tutorial about `Alternative`,
I\'ll just provide some examples
and intuitions gained throughout playing with it.

I happened to heard this typeclass by a quick scan on typeclassopedia,
and there are only few functions related to it, which I think
might not take much time
to try them all. These are all motivations about this article.

As always, **bold sentences** stands for my intuitions.

# "a monoid on applicative functors"

`Alternative` is a typeclass lied in `Control.Applicative`,
most of the time, I import this package to use functions like `<$>`, `<*>`
and data type `ZipList`, but no more.
I happened to heard this typeclass by a quick scan on typeclassopedia,
and there are only few functions related to it, which I think
might not take much time
to try them all. These are all motivations about this article.

```haskell
import Control.Applicative
```

First function in this class is called `empty`,
so I guess **`Alternative` is some typeclass that allows containing nothing**.
This would explain why there are so many `Applicative`s but
I can only see `[]` and `Maybe` being instances of this typeclass.

The document says `Alternative` is "a monoid on applicative functors".
So we can make an analogy between `(mempty, mappend)` from `Monoid`
and `(empty, <|>)` from `Alternative`. Let\'s try it out:

```haskell
v1 :: [String]
v1 = empty <|> empty <|> empty

v2 :: [String]
v2 = v1 <|> pure "foo" <|> pure "bar"

v3 :: Maybe Int
v3 = pure 10 <|> pure 20

v4 :: Maybe Char
v4 = empty <|> pure 'A' <|> undefined

v5 :: Maybe ()
v5 = empty <|> empty <|> empty
```

Let\'s bring up GHCi:

```haskell
λ> v1
[]
λ> v2
["foo","bar"]
λ> v3
Just 10
λ> v4
Just 'A'
λ> v5
Nothing
```

`empty` works like what we expected, and `<|>` for lists seems straightforward.
But when it comes to `Maybe`, we find that only the first non-`Nothing` one takes effect,
if any.

# `some` and `many`

The type for `some` and `many` are identical,
so just fill in some instances I come up with on the fly:

```haskell
v6 :: [[String]]
v6 = some ["not working"]

v7 :: [[String]]
v7 = some empty
```

But when I try this out on GHCi, something is going wrong:

```haskell
λ> v6
^CInterrupted.
λ> v7
[]
```

I gave `v6` some time to run but it didn\'t terminate, so I
cancelled it by hand.

By looking at the document, I find some clue: `some` and `many`
seem to look for some solutions to the following equations:

```haskell
some v = (:) <$> v <*> many v
many v = some v <|> pure []

-- rewrite to break the recursive relation will help?
some v = (:) <$> v <*> (some v <|> pure [])
many v = ((:) ($) v <*> many v) <|> pure []
```

I can\'t go any further from here, but doing some research, I find the following links that
might help, but for now I just leave this two functions as mysteries.
(Maybe it\'s just something like [`fix`](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Function.html#v:fix), interesting, but IMHO not useful ).

Related links:

* [What are Alternative's “some” and “many” useful for?](http://stackoverflow.com/questions/18108608/what-are-alternatives-some-and-many-useful-for)
* [Functions from 'Alternative' type class](http://stackoverflow.com/questions/7671009/functions-from-alternative-type-class)

# `optional`

For the rest of the story, I\'ll just type them directly into GHCi.

The last function about `Alternative` is `optional`,
Let\'s check its type and feed it with some instances:

```haskell
λ> :t optional
optional :: Alternative f => f a -> f (Maybe a)
λ> optional Nothing
Just Nothing
λ> optional $ Just 1
Just (Just 1)
λ> optional []
[Nothing]
λ> optional [1,2,3]
[Just 1,Just 2,Just 3,Nothing]
```

Wait a minute, is `f a -> f (Maybe a)` looks familiar to you?
It looks like an unary function that has type `a -> Maybe a` under some contexts.
I think the simplest expression that matches this type would be `(Just <$>)`.
Let\'s do the same thing on it.

```haskell
λ> :t (Just <$>)
(Just <$>) :: Functor f => f a -> f (Maybe a)
λ> (Just <$>) Nothing
Nothing
λ> (Just <$>) Just 1
Just (Just 1)
λ> (Just <$>) []
[]
λ> (Just <$>) [1,2,3]
[Just 1,Just 2,Just 3]
```

Comparing the output of `optional` and `(Just <$>)`,
we find that `optional` will attach an `empty` to the end of this monoid.
And that `empty` would be the "none" part from `optional`\'s description:
"One or none". In addition, we\'ve seen `<|>` is the binary operation for this monoid,
so we can have a guess:

```
optional v = (Just <$> v) <|> pure Nothing
```

And this turns out to be the exact implementation.

# Summary

Not much is done in this post, I have to admit that type-tetris
is not always the best way.
As an afterthought, I didn\'t know how `Alternative` will be used
so there was little hint that I can rely on when I was trying
to figure out `many` and `some`.

Anyway, if happened to read all contents in this article,
sorry for wasting your time and thanks for your patience.
