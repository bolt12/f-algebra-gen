{-# LANGUAGE TypeOperators #-}

-- (c) MP-I (1998/9-2006/7) and CP (2005/6-2018/9)

module Cp where

infixl 5  ><
infixl 4  -|-

-- (1) Product -----------------------------------------------------------------

-- Type alias
type a >< b = (a, b)

split :: (a -> b) -> (a -> c) -> a -> b >< c
split f g x = (f x, g x)

(><) :: (a -> b) -> (c -> d) -> a >< c -> b >< d
f >< g = split (f . p1) (g . p2)

-- the 0-adic split 

(!) :: a -> ()
(!) = const ()

-- Renamings:

p1 :: a >< b -> a
p1 = fst

p2 :: a >< b -> b
p2 = snd

-- (2) Coproduct ---------------------------------------------------------------

-- Type alias

type a -|- b = Either a b

-- Renamings:

i1 :: a -> a -|- b
i1 = Left

i2 :: b -> a -|- b
i2 = Right

-- either is predefined

(-|-) :: (a -> b) -> (c -> d) -> a -|- c -> b -|- d
f -|- g = either (i1 . f) (i2 . g)

-- McCarthy's conditional:

cond :: (b -> Bool) -> (b -> c) -> (b -> c) -> b -> c
cond p f g = either f g . grd p

-- (3) Exponentiation ---------------------------------------------------------

-- curry is predefined

ap :: (a -> b) >< a -> b
ap = uncurry ($)

expn :: (b -> c) -> (a -> b) -> a -> c
expn f = curry (f . ap)

p2p :: a >< a -> Bool -> a
p2p p b = if b then snd p else fst p -- pair to predicate

-- exponentiation functor is (a->) predefined 

-- instance Functor ((->) s) where
--	fmap f g = f . g

-- (4) Others -----------------------------------------------------------------

--const :: a -> b -> a st const a x = a is predefined

grd :: (a -> Bool) -> a -> a -|- a
grd p x = if p x then Left x else Right x

-- (5) Natural isomorphisms ----------------------------------------------------

swap :: a >< b -> b >< a
swap = split p2 p1

assocr :: ((a >< b) >< c) -> (a >< (b >< c))
assocr = split (p1 . p1) (p2 >< id)

assocl :: (a >< (b >< c)) -> ((a >< b) >< c)
assocl = split (id >< p1) (p2 . p2)

undistr :: (a >< b) -|- (a >< c) -> a >< (b -|- c)
undistr = either (id >< i1) (id >< i2)

undistl :: (b >< c) -|- (a >< c) -> (b -|- a) >< c
undistl = either (i1 >< id) (i2 >< id)

flatr :: (a >< (b >< c)) -> (a, b, c)
flatr (a, (b, c)) = (a, b, c)

flatl :: ((a >< b) >< c) -> (a, b, c)
flatl ((b, c), d) = (b, c, d)

-- pwnil = split id (!)

br :: a -> a >< ()
br = split id (!) -- bang on the right, old pwnil means "pair with nil"

bl :: a -> () >< a
bl = swap . br

coswap :: a -|- b -> b -|- a
coswap = either i2 i1

coassocr :: ((a -|- b) -|- c) -> (a -|- (b -|- c))
coassocr = either (id -|- i1) (i2 . i2)

coassocl :: (b -|- (a -|- c)) -> ((b -|- a) -|- c)
coassocl = either (i1 . i1) (i2 -|- id)

distl :: ((c -|- a) >< b) -> (c >< b) -|- (a >< b)
distl = uncurry (either (curry i1) (curry i2))

distr :: (b >< (c -|- a)) -> (b >< c) -|- (b >< a)
distr = (swap -|- swap) . distl . swap

-- (6) Class bifunctor ---------------------------------------------------------

class BiFunctor f where
      bmap :: (a -> b) -> (c -> d) -> (f a c -> f b d)

instance BiFunctor Either where
    bmap f g = f -|- g

instance BiFunctor (,) where
    bmap f g  = f >< g

-- (7) Monads: -----------------------------------------------------------------

-- (7.1) Kleisli monadic composition -------------------------------------------

infix 4  .!

(.!) :: Monad a => (b -> a c) -> (d -> a b) -> d -> a c
(f .! g) a = g a >>= f

mult :: (Monad m) => m (m b) -> m b
-- also known as join
mult = (>>= id)

-- (7.2) Monadic binding ---------------------------------------------------------

ap' :: (Monad m) => (a -> m b, m a) -> m b
ap' = uncurry (=<<)

-- (7.3) Lists

singl :: a -> [a]
singl = return

-- (7.4) Strong monads -----------------------------------------------------------

class (Functor f, Monad f) => Strong f where
      rstr :: (f a >< b) -> f (a >< b)
      rstr (x, b) = do a <- x ; return (a, b)

      lstr :: (b >< f a) -> f (b >< a)
      lstr(b, x) = do a <- x ; return (b, a)

instance Strong IO

instance Strong []

instance Strong Maybe

dstr :: Strong m => (m a, m b) -> m (a, b)       --- double strength
--dstr = mult . fmap rstr . lstr
dstr = rstr .! lstr

splitm :: Strong ff => ff (a -> b) -> a -> ff b
-- Exercise 4.8.13 in Jacobs' "Introduction to Coalgebra" (2012)
splitm = curry (fmap ap . rstr)

{--
-- (7.5) Monad transformers ------------------------------------------------------

class (Monad m, Monad (t m))  => MT t m where   -- monad transformer class
      lift :: m a -> t m a

-- nested lifting:

dlift :: (MT t (t1 m), MT t1 m) => m a -> t (t1 m) a
dlift = lift . lift

--}

-- (8) Basic functions, abbreviations ------------------------------------------

bang :: a -> ()
bang = (!)

dup :: c -> c >< c
dup = split id id

zero :: b -> Integer
zero = const 0

one :: b -> Integer
one  = const 1

nil :: b -> [a]
nil = const []

cons :: (a >< [a]) -> [a]
cons = uncurry (:)

add :: (Integer >< Integer) -> Integer
add = uncurry (+)

mul :: (Integer, Integer) -> Integer
mul = uncurry (*)

conc :: ([a] >< [a]) -> [a]
conc = uncurry (++)

true :: b -> Bool
true = const True

nothing :: b -> Maybe a
nothing = const Nothing

false :: b -> Bool
false = const False

inMaybe :: () -|- a -> Maybe a
inMaybe = either (const Nothing) Just

-- (9) Advanced ----------------------------------------------------------------

class (Functor f) => Unzipable f where
      unzp :: f (a >< b) -> (f a >< f b)
      unzp = split (fmap p1) (fmap p2)

class Functor g => DistL g where
      lamb :: Monad m => g (m a) -> m (g a)

instance DistL [] where lamb = sequence

instance DistL Maybe where
      lamb Nothing  = return Nothing
      lamb (Just a) = fmap Just a  -- where mp f = (return.f).!id

aap :: Monad m  => m (a->b) -> m a -> m b
-- to convert Monad into Applicative
-- (<*>) = curry(lift ap) where lift h (x,y) = do { a <- x; b <- y; return ((curry h a b)) }
aap mf mx = do f <- mf ; f <$> mx

-- gather: n-ary split

gather :: [a -> b] -> a -> [b]
gather l x = map ($ x) l

-- the dual of zip

cozip :: (Functor f) => f a -|- f b -> f (a -|- b)
cozip = either (fmap Left) (fmap Right)

--------------------------------------------------------------------------------
tot :: (a -> b) -> (a -> Bool) -> a -> Maybe b
tot f p = cond p (return . f) nothing
--------------------------------------------------------------------------------
