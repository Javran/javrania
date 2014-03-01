---
layout: post
title: Type tetris and typeclassopedia
tags: type-tetris, typeclassopedia, haskell
---

# Let\'s make our way to Haskell

I\'ve become one interested in Haskell for quite a while,
but I think the only book I\'ve read about Haskell is
[LYAH](http://learnyouahaskell.com/). For more advanced topics,
I just read articles from both
[haskell.org](http://www.haskell.org/haskellwiki/Introduction)
and
[Haskell wikibooks](https://en.wikibooks.org/wiki/Haskell).
But as I dig deeper into more genius concepts and ideas in Haskell,
things become complicated and mind-blowing and the biggest obstacle
for me is that I have little understanding about types,
which is ubiquitous in the world of Haskell.

Today I\'d like to share two interesting stuffs which, IMHO,
would help you to learn more about Haskell: "type-tetris" and "typeclassopedia".

# Type-tetris

Just few days ago I came across
[this link](http://www.reddit.com/r/haskell/comments/1yvfmc/programming_with_types_not_tutorials/), which eventually gave me an article:
[Programming With Types, Not Tutorials](https://www.fpcomplete.com/user/chowells79/types-not-tutorials).
In this article, the author shows us how to make Haskell code work as expected
while have least understanding about
underlying complex mathematical concepts.
After reading this article, I realized that
it might not be our responsibility to get full knowledge about
"how does it work",
but we should really focus on "how to make it work".

I think this is the beauty of Haskell: thanks to its strong and statical type system,
when some Haskell type-checks,
we can almost be confident to say that it should work as expected.
On the other hand, we should also be as lazy as Haskell in some sense,
since no one puts a gun in your head and say
"you should figure out how exactly it works", we can just leave all the
underlying math and proofs unexplored until we need it.

Here I collect some comments on the previous Reddit link:

> drb226: This is what I like to call "type tetris."
> It\'s fun, and you end up with correct programs
> without even having to understand what you just assembled. ...

> camccann: I got through most of my physics courses
> by relying on dimensional analysis and hoping that
> if the units were right, so was the equation.
> It worked surprisingly often.

If I understand correctly, as I\'ve previous said in my twitter,
the type itself is capable of carrying constraints with it,
So types have already embeded with properties and proofs,
and we can use types happily
without knowing too much about what is going on.

I like the idea of "type tetris": let\'s just get started
by using functions without "eagerly" understanding it.
And see if we can grab some ideas when we are playing with it.
By doing this, we focus more on "how to make it work" rather
than "how it works".

# Typeclassopedia

In addition, I happen to know that
there is a totally different way of learning Haskell:
[Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia).
This is an article recommended by many people in Stack Overflow,
which covers some basic understanding of how types are related to each other.
By reading this article, we are trying to answer the question of
"how it works" but are less interested in "how to make it work".

# Summary

So here I\'ve shown you two (potential) ways of learning Haskell
and fortunately you can take both ways and hope the experience
gained from both sides would help each other and bring you to
a higher level understanding of Haskell.

For myself, as a starting point of my type-tetris,
I explored a little about
[Free Monad](http://stackoverflow.com/questions/13352205/what-are-free-monads)
few days about, hopefully I\'ll write up another article in weekends
to share the code and my thoughts.
For typeclassopedia, I haven\'t read much. A potential plan would be
to play "type-tetris" with stuffs mentioned in the typeclassopedia first,
and then read the corresponding chapter to shed some light.
