-- Ch 1

square x = x*x

--1.1
quad x = square (square x)
quad' = square . square

max' x y 
    | x > y     = x
    | otherwise = y

areaOfCircle r = 2 * pi * r

-- "Programming in a functional language consists of building definitions and using the computer to evaluate expressions."
-- an _expression_ is used solely to describe/denote a value. 
-- The meaning of an expression _is_ its value. No side effects in getting the value
-- Expression can consist of other expressions. Order expressions reduced doens't matter; as no side-effects. e.g. square (3+4) could => square 7 [by +] or => (3+4) * (3+4) [by square]
-- => means "reduces to"
-- a definition _binds_ a name with a value.
-- Synonyms: reduce, evaluate, simplify. Expressions get reduced to their simplest, _canoncial_ form, also known as _normal form_.
-- Variables: not "boxes" holding a value which can change like OO. Variables are parts of definition that can vary, like a maths forula. In `square x = x*x`, the `x` can vary and the definition holds. But the `x` itself will not change (immutable).
-- Referential transparency: variables denote the same quantity provided we remain in the same definitions associated with them.
-- Expressions can be evaluated by substitution and simplification, using both primitive  rules and rules supplied by programmer as definitions
-- expression is a representation of a value, not the value itself. 49 can be expressed as XLIX (roman nums) or 7 x 7 (expression)
-- Symbol: ⊥, pronounce "bottom", means undefined. Similar to ∞ in calculus. 1/0 = ⊥.
--
-- reducing square (square (3+7))
-- 1. => square (square (10) )      -- by +
--    => square (10 * 10)           -- by square
--    => square (100)               -- by *
--    => 100*100                    -- by square
--    => 10000                      -- by *
--
-- 2. => (square (3+7)) * (square (3+7)) -- by square
--    => ((3+7)*(3+7)) * ((3+7) * (3+7)  -- by square
--
--
--type variables == polymorphic. e.g. f :: Int -> Int not polymorphic. g :: a -> a is.
--
-- case altneratives are called _guards_. e.g. max x y | x > y = x | otherwise . Predicates are guards.
-- function application is left-associative, to support currying. e.g. f x y = (f x) y. So f :: a -> b -> c is actually f :: a -> (b -> c). We can also define f (x,y), which means f :: (a, b) -> c (doesn't support partial app)

--1.4.3
sign :: (Num a, Ord a) => a -> Integer
sign x | (x<0) = -1
       | (x>0) = 1
       | otherwise = 0

--1.5.1
-- example: increase x > square x for all x
-- isSquare x = (exists y such that square y = x)
--
-- let y = intsqrt x
-- isSquare x = (square y = x)
-- So this will satisfy the spec
-- 
-- 1.5.2
-- intsqrt x <= sqrt x and > (sqrt (x-1)) 
--  or
-- intsqrt x = y where square y <= x and square (y+1) > x
--
-- impl (fractional?)
-- intsqrt x = last . takeWhile (\y -> y^2 <= x) $ [0..]
--


-- CHAPTER 2
-- Bracketed operator is called a _section_. e.g. (+). Can be used in prefix (+) 1 2, and is curried.
-- Can give a section one argument: so (+2) or (2+).
--
improve x y = (y + x/y)/2
satis x y = abs (y^2 - x) < eps where eps = 0.0001

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x | p x = x
             | otherwise = until' p f (f x)

sqrt' x = until' (satis x) (improve x) x


sqrt'' x = until' satis improve x
            where satis y = abs (y^2-x) < eps
                  improve y = (y+x/y)/2
                  eps = 0.00001 

norm (x,y) = (u `div` d, v `div` d)
                where u = (sign y) * x
                      v = abs y
                      d = gcd (abs u) v

-- function composition (.). (f . g) x = f ( g x )
---- associative. So (f . g) . h = f . (g . h)
-- inverse functions
-- strict functions. If f ⊥ = ⊥, then f is _strict_.
---- three x = 3       is not strict (always returns 3, even if x is undefined. e.g. three (1/0))
---- Can be strict in some args, not in others. e.g. `cond p x y = if p then x else y` is strict in its first argument (p). If p is true does not require y, if not does not require x.
-- Consider `fst (x,y)`. Eager-evaluation tries to reduce (x,y) first. Lazy applies `fst` first.
--
-- 2.6.1
-- h x y = f ( g x y )
--  RHS = (f . (g x)) y
--
-- Type inference rules:
--  (i) (application rule )If f x :: t, then x :: t'and f::t' -> t for some new type t';
--  (ii) (Equality rule) If both the types x :: t and x : : t' can be deduced for a
--  variable x , then t = t';
--  (iii) (Function rule ) If t -> u = t' -> u', then t = t' and u = u'.
--
-- 2.8.1
-- const x y = x
-- const :: t1 -> t2 -> t3
-- const :: t1 -> t2 -> t1 (by equality rule)
--
-- subst f g x = f x (g x)
-- subst :: t1 -> t2 -> t3 -> t4
--      f x (g x) :: t4
--              f :: t5 -> t6 -> t7
--              x :: t5
--            g x :: t6
--              g :: t5 -> t6
--
--  subst :: (t5 -> t6 -> t7) -> (t5 -> t6) -> t5 -> t4
--  subst :: (a -> b -> c) -> (a -> b) -> a -> c
--
--
-- Exercise: derive type of "map map"
-- map :: (a -> b) -> [a] -> [b]
-- map :: (a -> b) -> ([a] -> [b])     -- by right-assoc of application
-- Say we apply a function f :: c -> d
-- map f :: [c] -> [d]
-- Then if we use map for f
-- map map :: [(a -> b)] -> [[a] -> [b]]
--   (with help from http://lambda-the-ultimate.org/node/2948)
--
-- Exercise: filter in terms of (concat . map box)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = concat . map box
            where box x = if p x then [x] else []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = concat . map box
            where box x = [ i | i <- [x], p x]

-- 5.1.1
-- 0 + n = n = n + 0 by induction
--
-- For n = 0: 0 + 0 = 0 by hypothesis
-- Hyp: n=k: 0+k=k=k+0
-- For n=k+1:
--      0+(k+1)=(k+1)=(k+1)+0
--      (0+k)+1=k+1 (by assoc)
--      k+1=k+1 (by hyp), so true for LHS
--      RHS:
--      k+1=k+1+0
--      k+1=k+1 (by +), so true for rhs
--  So hypothesis holds for all k.

-- symetrical zip (with undef) vs std def
zip' [] _ = []
zip' (x:xs) [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


-- HEAD NORMAL FORM
-- Redex = reducible expression
-- Normal form 
--  = all terms/subterms reduced to canonical/normal form. 
--  = Can't be reduced further.
--  = not a redex
-- Head normal form 
--  = not a redex, and can't become a redex by reducing subterms.
--      (i.e. no reduction rules will reduce that expression)
--  = e.g. e1:e2, even if e1 and e2 can be reduced, the basic x:y term cannot.
--    (1+2):[] is in HNF. Normal form would be 3:[]. Can't reduce the (:) bit.
--    Other example is (e1, e2).
-- All normal form expr are HNF. But not all HNF is normal form.
--
--  



