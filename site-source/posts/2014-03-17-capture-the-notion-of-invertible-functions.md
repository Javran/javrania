---
layout: post
title: Capture the notion of invertible functions
tags: haskell
---

# Intro

This article is originally posted on [Code Review](http://codereview.stackexchange.com/questions/44550/capture-the-notion-of-invertible-functions). And I think this can also be a good post.

# Motivation

I find sometimes it is useful to capture the notion of invertible functions.

The idea is that if two functions `f :: a -> b` and `g :: b -> a` are the inverse function of each other, and if there is another function `h :: b -> b`, then `h` can also work on values of type `a`.

Moreover, if there`f'` and `g'` are another pair of functions that are the inverse function of each other, `(f,g)` and `(f',g')` can actually be composed to `(f' . f, g . g')` and the invertibility still holds.

The following is my attempt to implement this in haskell, and I'm wondering if an existing library can do the same thing (or even more general thing) for me.
Also advice and comments about my code are appreciated.

# Implemnetation

First I use records to store two functions:

```hasekll
data Invertible a b = Invertible
    { into :: a -> b
    , back :: b -> a
    }
```

`into` means "convert a into b" while `back` means "convert b back to a".

And then few helper functions:

```haskell
selfInv :: (a -> a) -> Invertible a a
selfInv f = Invertible f f

flipInv :: Invertible a b -> Invertible b a
flipInv (Invertible f g) = Invertible g f

borrow :: Invertible a b -> (b -> b) -> a -> a
borrow (Invertible fIn fOut) g = fOut . g . fIn

liftInv :: (Functor f) => Invertible a b -> Invertible (f a) (f b)
liftInv (Invertible a b) = Invertible (fmap a) (fmap b)
```

In the above code `borrow` will use the pair of functions to make its last argument `g`
available to values of type `a`. And changing `borrow f` to `borrow (flipInv f)`
will make `g` available to values of type `b`.
Therefore `borrow` captures my initial idea of
making a function of type `b -> b` available for values of `a`
if `a` and `b` can be converted to each other.

In addition, `Invertible` forms a monoid-like structure,
I use `rappend` and `rempty` to suggest a similiarity between it and `Monoid`:

```haskell
rempty :: Invertible a a
rempty = selfInv id

rappend :: Invertible a b
        -> Invertible b c
        -> Invertible a c
(Invertible f1 g1) `rappend` (Invertible f2 g2) =
    Invertible (f2 . f1) (g1 . g2)
```

# Examples

Here I have two examples to demonstrate that `Invertible` might be useful.

## Data Encryption

It is natural that `Invertible` can be used under scenario of symmetric encryption. `Invertible (encrypt key) (decrypt key)` might be one instance if:

```haskell
encrypt :: Key -> PlainText -> CipherText
decrypt :: Key -> CipherText -> PlainText
```

To simplify a little, I make an example of [Caesar cipher](https://en.wikipedia.org/wiki/Caesar_Cipher) and assume that plain text contains only uppercase letters:

```haskell
-- constructor should be invisible from outside
newtype OnlyUpper a = OnlyUpper
    { getOU :: [a]
    } deriving (Eq, Ord, Show, Functor)

ouAsList :: Invertible (OnlyUpper a) [a]
ouAsList = Invertible getOU OnlyUpper

onlyUpper :: String -> OnlyUpper Char
onlyUpper = OnlyUpper . filter isAsciiUpper

upperAsOrd :: Invertible Char Int
upperAsOrd = Invertible ord' chr'
    where
        ord' x = ord x - ord 'A'
        chr' x = chr (x + ord 'A')
```

And Caesar Cipher is basically doing some modular arithmetic:

```haskell
modShift :: Int -> Int -> Invertible Int Int
modShift base offset = Invertible f g
    where
        f x = (x + offset) `mod` base
        g y = (y + (base - offset)) `mod` base

caesarShift :: Invertible Int Int
caesarShift = modShift 26 4

caesarCipher :: Invertible (OnlyUpper Char) (OnlyUpper Char)
caesarCipher = liftInv (upperAsOrd
                       -- Char <-> Int
                       `rappend` caesarShift
                       -- Int <-> Int
                       `rappend` flipInv upperAsOrd)
                       -- Int <-> Char
```

One way to use `Invertible` is
just using its `into` and `back` as `encrypt` and `decrypt`,
and `Invertible` also gives you the power of manipulating encrypyed data
as if it was plain text:

```haskell
exampleCaesar :: IO ()
exampleCaesar = do
    let encF = into caesarCipher
        decF = back caesarCipher
        encrypted = encF (onlyUpper "THEQUICKBROWNFOX")
        decrypted = decF encrypted
        encrypted' = borrow (flipInv caesarCipher
                             `rappend` ouAsList) (++ "JUMPSOVERTHELAZYDOG") encrypted
        decrypted' = decF encrypted'

    print encrypted
    -- OnlyUpper {getOU = "XLIUYMGOFVSARJSB"}
    print decrypted
    -- OnlyUpper {getOU = "THEQUICKBROWNFOX"}

    print encrypted'
    -- OnlyUpper {getOU = "XLIUYMGOFVSARJSBNYQTWSZIVXLIPEDCHSK"}
    print decrypted'
    -- OnlyUpper {getOU = "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG"}
```

## Matrix manipulation

Sometimes it's convenient to write some code that manipulates matrices using `Invertible`.

Say there is a list of type `[Int]` in which `0` stands for an empty cell,
and we want every non-zero element move to their leftmost possible position
while preserving the order:

```haskell
compactLeft :: [Int] -> [Int]
compactLeft xs = nonZeros ++ replicate (((-) `on` length) xs nonZeros) 0
    where nonZeros = filter (/= 0) xs
```

Now consider 2D matrices, we want to "gravitize" the matrix so that every non-zero element
in it falls to {left,right,up,down}-most possible position while preserving the order.

```haskell
data Dir = DU | DD | DL | DR deriving (Eq, Ord, Enum, Show, Bounded)
gravitizeMat :: Dir -> [[Int]] -> [[Int]]
gravitizeMat dir = borrow invertible (map compactLeft)
    where mirrorI = selfInv (map reverse)
        diagonalI = selfInv transpose
        invertible = case dir of
            DL -> rempty
            DR -> mirrorI
            DU -> diagonalI
            DD -> diagonalI `rappend` mirrorI
```

here `Invertible` comes into play by the observation that `transpose` and `map reverse`
are all invertible (moreover, they are inverse functions of themselves).
So that we can tranform matrices and pretend the problem is only "gravitize to the left".

Here is one example:

```haskell
print2DMat :: (Show a) => [[a]] -> IO ()
print2DMat mat = do
    putStrLn "Matrix: ["
    mapM_ print mat
    putStrLn "]"

exampleMatGravitize :: IO ()
exampleMatGravitize = do
    let mat = [ [1,0,2,0]
              , [0,3,4,0]
              , [0,0,0,5]
              ]
    print2DMat mat

    let showExample d = do
            putStrLn $ "Direction: " ++ show d
            print2DMat $ gravitizeMat d mat

    mapM_ showExample [minBound .. maxBound]
```

And the result will be:

```
Matrix: [
[1,0,2,0]
[0,3,4,0]
[0,0,0,5]
]
Direction: DU
Matrix: [
[1,3,2,5]
[0,0,4,0]
[0,0,0,0]
]
Direction: DD
Matrix: [
[0,0,0,0]
[0,0,2,0]
[1,3,4,5]
]
Direction: DL
Matrix: [
[1,2,0,0]
[3,4,0,0]
[5,0,0,0]
]
Direction: DR
Matrix: [
[0,0,1,2]
[0,0,3,4]
[0,0,0,5]
]
```

# Complete code

You can find my complete code from [gist](https://gist.github.com/Javran/9593215).
