---
layout: post
title: Comonad, Zipper and Conway's Game of Life (Part 2)
tags: haskell, comonad, zipper, type-tetris
---

# Intro

In the [previous post](2014-04-13-comonad-and-conways-game-of-life.html), We\' ve discussed a little bit about comonad.
But these abstract concepts are useless without practical usage.

Forget about comonad and Conway\'s Game of Life for a while.
Today I want to show you an interesting example, which will give you
some ideas about what it means by saying "the value of one place depends
on the value of its neighborhoods". And these examples will be connect to
the concept of comonad in a future post.

# 2D Wave Propagation

This example simulates a simple wave propagation by ASCII art.

The "world" is represented by a list of characters,
each of which has a special meaning, namely:

* `<space>` : Just the medium. Air, water, etc.
* `>` : a wave moving right.
* `<` : a wave moving left.
* `X` : two waves with opposite direction meeting each other.
* `*` : a wave source which will disappear at the next moment, producing
waves in both directions.

The simulation of wave propagation will be archieved by continuously printing
the next generation of the "world".

For example, if we are given a string: `"*  >  *   *  <  **<"`, the output
would be the following:

```
*  >  *   *  <  **<  
 >  >< > < ><  <<X>  
  > <>  X  <> <<< >> 
   X  >< ><  X<<   >>
  < > <> <> <<X     >
 <   X  X  X<< >     
<   < >< ><<X   >
...
```

# Propagation Rules

I believe it\'s easy to see the pattern. And you can write a function
to describe the rule.
At the first glance you might think the state of a fixed position
in the "world" only depends on its previous state and the previous states
of its neighborhoods. But it turns out the previous state of itself isn\'t
necessary, but we just leave it as an argument (Simply because I think
it looks better).

```haskell
waveRule :: Char -> Char -> Char -> Char
waveRule _ l r
    | fromL && fromR = 'X'
    | fromL          = '>'
    | fromR          = '<'
    | otherwise      = ' '
    where
        fromL = l `elem` ">*X"
        fromR = r `elem` "<*X"
```

# First Solution

(This part is not about zippers or comonads, feel free to skip it)
It is not hard to come up with a solution involving only basic list manipulations.
I think it would be a good exercise.
My solution can be found [here](https://gist.github.com/Javran/eed695b4f837cc8ea214).

The output should be:

```
*  >  *   *  <  **<
 >  >< > < ><  <<X>
  > <>  X  <> <<< >
   X  >< ><  X<<   
  < > <> <> <<X    
 <   X  X  X<< >   
<   < >< ><<X   >  
   <  <> <X< >   > 
  <  <  X<<>  >   >
 <  <  <<X  >  >   
<  <  <<< >  >  >  
  <  <<<   >  >  > 
 <  <<<     >  >  >
<  <<<       >  >  
  <<<         >  > 
 <<<           >  >
<<<             >  
<<               > 
<                 >
                   
```

# A Limited View of the World

Now suppose this 2D world is infinite on both directions,
and we introduce the obvious coordinate system into this world.
We will no longer see the whole world, but only a portion of it.

Now we are given two coordinates, and we can only observe the world
in between these coordinates, can we still work out the problem?

It turns out pretty difficult to reuse the previous approach because:

* Lists can only be infinite in one direction while
we need a world representation that can be infinite in both directions
so that we are allowed to theoretically view
the world in between two arbitrary coordinates.

* Given a world and its previous generation, it is hard to find the
"old cell state" or "old neighboring cell states"
unless we can introduce something like coordinates to establish
the corrspondence between cells.

* We don\'t know where to start generating the next iteration as
the world can be infinite in both directions. We can\'t simply walk
through it from left to right, which might not terminate.

I\'d recommend to use a list [zipper](http://en.wikipedia.org/wiki/Zipper_%28data_structure%29)
to overcome these problems.

# List Zippers

Zippers are a way to efficiently walk back and forth or update values in certain data structures
like lists and trees. Those data structures can usually be traversed in a deterministic way.

A zipper of a certain data structure usually consists of two parts:
a stack of data contexts (each data context is an incompete data structure with a "hole" in it),
and a value that can fill a "hole".

Here we are only interested in list zippers. But there are plenty of useful tutorials about zippers
like [this one](http://learnyouahaskell.com/zippers)
from [LYAH](http://learnyouahaskell.com/chapters).

To explain what we just said about zippers, we take a random list `[1,2,3]`
(you should recall that `[1,2,3]` is just a shorthand for `1:2:3:[]`)
and traverse it to demonstrate list zippers.

| Stack | Focus | Zipper = (Stack,Focus) |
| --- | --- | --- |
| `[]` | `[1,2,3]` | `([],[1,2,3])` |
| `[1:<hole>]` | `[2,3]` | `([1],[2,3])` |
| `[2:<hole>, 1:<hole>]` | `[3]` | `([2,1],[3])` |
| `[3:<hole>, 2:<hole>, 1:<hole>]` | `[]` | `([3,2,1],[])` |

A list zipper changes as we are walking in the data structure,
the table above shows how the list zipper changes as we walk though
a list from left to right. Note that since the data context for a list is always
something like `(<value>:<hole>)`, we can simply represent it as `<value>`.
That is why a list zipper are usually represented as a pair of two lists,
or to be more precise, a stack and a list.

The data context stack makes it possible to traverse backwards.
Whenever we want to do so, pop one data context from the stack,
and fill in its hole by using the current focus.
For example, to go backwards when the list zipper is `([2,1],[3])`,
we pop the data context to get `2:<hole>`, fill in the hole with
our current focus, namely `[3]` and we end up with `([1],2:[3])` whose
focus is changed from `[3]` to `[2,3]`.

We can also change the value at the focus efficiently. For example,
when the list zipper is `([2,1],[3])`, we modify the focus to `[4,5,6]`.
And then we keep going backwards to recover the list.
We will end up with `1:2:[4,5,6]` and as you can see the part we were
focusing on (namely `[3]`) is now replaced by `[4,5,6]`.

# List Zippers to the Rescue

With some introduction of zippers, I can now explain
how can list zippers solve our problem.

* List zippers can be infinite in both directions by using a simple trick:
make the context stack infinite. It is an importation observation that
the stack in the list zipper are actually the reversed left part of the list
and the focus the right part. By making both the reversed left part and right part
infinite, we end up with a list zipper that is infinite in both directions.

* It\'s quite easy to find "old cell state" and "old neighboring cell states"
given the list zipper. The old cell is the `head` of the current focus,
the cells right next to it are the top of the stack and the second element of the current
focus, respectively. Therefore for any given list zipper, we can yield the next cell state
of the `head` of the current focusing list.

* We don\'t need to worry about where to start generating the next world,
given a list zipper, we know how to iteratively
move the focus to the left or to the right. So as long as we can pin
one position to the origin point of the world, we can take steps
based on the original zipper by moving either left or right to focus on
the coordinate in question. And a list zipper contains sufficient
information to calculate the next cell state in question.

# Implementation

## Zippers

First let\'s begin with zipper implementations.
Since the world can never be empty, it is totally safe to
break the focusing data (`[a]`) into its components (`(a,[a])`).
By rearranging the order of arguments
(`([a],[a])` ... `([a],(a,[a]))` ... `LZipper a [a] [a]`)
we have our final version of `LZipper` here:

```haskell
{-# LANGUAGE DeriveFunctor #-}

-- | LZipper <current> <left (reversed)> <right>
data LZipper a = LZipper a [a] [a]
    deriving (Show, Functor)
```

Here the old focus would be `<current>:<right>` but we can break the focusing list
to make it looks nicer: now a list zipper in our case consists of three parts,
a current focus `<current>`, everything to the left of it `<left (reversed)>`
and everything to the right of it `<right>`.

With the list zipper definition given, it\'s easy to define basic operations:

```haskell
-- | shift left and right
zipperMoveL, zipperMoveR :: LZipper a -> LZipper a
zipperMoveL (LZipper a (x:xs') ys) = LZipper x xs' (a:ys)
zipperMoveL _ = error "Invalid move"
zipperMoveR (LZipper a xs (y:ys')) = LZipper y (a:xs) ys'
zipperMoveR _ = error "Invalid move"

-- | get the current focusing element
current :: LZipper a -> a
current (LZipper v _ _) = v
```

## Conversion between Limited Worlds and Infinite Worlds

To initialize the world we need to convert from a list of cells to a zipper
which represents the infinite world. This can be achieved by padding the list
to make it infinite in both directions:

```haskell
-- | initial world to a zipper
rangeToZipper :: a -> [a] -> LZipper a
rangeToZipper v wd = case wd of
    []   -> LZipper v pad pad
    x:xs -> LZipper x pad (xs ++ pad)
    where
        pad = repeat v
```

And to view a portion of the infinite world, we take as argument
two coordinates and a zipper
(the zipper is assumed to point to the origin point),
move the zipper to the position specified by one of the coordinate,
and then extract the value of the focus from zipper and keep moving the zipper
to the other coordinate.

```haskell
-- | a view range (coordinates), a zipper to a portion of the world
zipperToRange :: (Int, Int) -> LZipper a -> [a]
zipperToRange (i,j) zp = fmap current zippers
    where
        zippers = take (j - i + 1) (iterate zipperMoveR startZ)
        startZ = zipperMoveFocus i zp
        zipperMoveFocus :: Int -> LZipper a -> LZipper a
        zipperMoveFocus t z
            | t > 0     = zipperMoveFocus (t-1) (zipperMoveR z)
            | t < 0     = zipperMoveFocus (t+1) (zipperMoveL z)
            | otherwise = z
```

## Propagation Rules

We modify `waveRule` function above so that it can produce the
next cell state from a zipper. The nice thing about our zipper
is that both of the neighboring old cell states can be easily
found by pattern matching on arguments.

```haskell
waveRule :: LZipper Char -> Char
waveRule (LZipper _ (l:_) (r:_))
    | fromL && fromR = 'X'
    | fromL          = '>'
    | fromR          = '<'
    | otherwise      = ' '
    where
        fromL = l `elem` ">*X"
        fromR = r `elem` "<*X"
waveRule _ = error "null zipper"
```

And then we rush to complete the main function,
assuming `nextGen :: LZipper Char -> LZipper Char`,
a function that takes a zipper and produces a zipper of the next generation.
has been implemented for us.

```haskell
nextGen :: LZipper Char -> LZipper Char
nextGen = undefined

main :: IO ()
main = mapM_ (putStrLn . zipperToRange (-20,40)) (take 20 (iterate nextGen startZ))
    where
        startStr = "*  >  *   *  <  **<"
        startZ = rangeToZipper ' ' startStr
```

In the code above, we take 20 generations, view the world within range `(-20,40)`.

## The Final Missing Piece

The only thing missing in our implementation is the `nextGen` function,
this is also where the magic happens. Let\'s implement it step by step.

By taking its type signature into account, we can write down the shape of the body:

```haskell
nextGen :: LZipper Char -> LZipper Char
nextGen z = LZipper c' ls' rs'
    where
        c' = undefined
        ls' = undefined
        rs' = undefined
```

And it\'s not hard to figure out what is `c'` -- the new cell state in correspondence with `c`:

```haskell
nextGen :: LZipper Char -> LZipper Char
nextGen z = LZipper c' ls' rs'
    where
        c' = waveRule z
        ls' = undefined
        rs' = undefined
```

To figure out `ls'`, we first try to figure out the first element of it, namely `l'`:

```haskell
nextGen :: LZipper Char -> LZipper Char
nextGen z = LZipper c' ls' rs'
    where
        c' = waveRule z
        l' = undefined
        ls' = l' : undefined
        rs' = undefined
```

Since the focus of `l'` is the direct neighborhood of `c`, we can simply move the zipper
to calculate its new state:

```haskell
nextGen :: LZipper Char -> LZipper Char
nextGen z = LZipper c' ls' rs'
    where
        c' = waveRule z
        l' = waveRule . zipperMoveL $ z
        ls' = l' : undefined
        rs' = undefined
```

Comparing `c'` and `l'`, we can find the pattern:

```haskell
nextGen :: LZipper Char -> LZipper Char
nextGen z = LZipper c' ls' rs'
    where
        c' = waveRule z
        l' = waveRule . zipperMoveL $ z
        ls' = [waveRule . zipperMoveL $ z, waveRule . zipperMoveL . zipperMoveL $ z, ...]
        rs' = undefined
```

And the same pattern holds for `rs'`: we just keep moving the zipper to its left or right,
and produce new states by applying `waveRule` to it. So we end up with:

```haskell
nextGen :: LZipper Char -> LZipper Char
nextGen z = LZipper c' ls' rs'
    where
        c' = waveRule z
        ls' = map waveRule . tail $ iterate zipperMoveL z
        rs' = map waveRule . tail $ iterate zipperMoveR z
```

Now the whole program should be complete, if you run it, you will get something
like this:

```
                    *  >  *   *  <  **<                      
                   < >  >< > < ><  <<X>                      
                  <   > <>  X  <> <<< >>                     
                 <     X  >< ><  X<<   >>                    
                <     < > <> <> <<X     >>                   
               <     <   X  X  X<< >     >>                  
              <     <   < >< ><<X   >     >>                 
             <     <   <  <> <X< >   >     >>                
            <     <   <  <  X<<>  >   >     >>               
           <     <   <  <  <<X  >  >   >     >>              
          <     <   <  <  <<< >  >  >   >     >>             
         <     <   <  <  <<<   >  >  >   >     >>            
        <     <   <  <  <<<     >  >  >   >     >>           
       <     <   <  <  <<<       >  >  >   >     >>          
      <     <   <  <  <<<         >  >  >   >     >>         
     <     <   <  <  <<<           >  >  >   >     >>        
    <     <   <  <  <<<             >  >  >   >     >>       
   <     <   <  <  <<<               >  >  >   >     >>      
  <     <   <  <  <<<                 >  >  >   >     >>     
 <     <   <  <  <<<                   >  >  >   >     >>    
```

Let\'s call it a day here. In the next part we\'ll go back to comonads, and its relationship
between zippers. And hopefully we will finally see the implementation of Conway\'s Game of Life.

# Complete Code

You can find my complete code from [gist](https://gist.github.com/Javran/926296611a521cb00467).

# Other parts

* [Part 1](2014-04-13-comonad-and-conways-game-of-life.html)
