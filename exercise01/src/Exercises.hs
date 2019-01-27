{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Exercises where

import Data.Tuple (swap)
import Data.Monoid

{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountNil  :: CountableList
  CountCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountNil = 0
countList (CountCons a as) = count a + countList as
             

-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountNil = CountNil
dropZero (CountCons a as) = if count a > 0
                          then (CountCons a (dropZero as))
                          else dropZero as

-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

-- We can't because we can't pattern match on the original constructor

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil  :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

-- sure
reverseAnyList :: AnyList -> AnyList
reverseAnyList (AnyCons a as) = undefined
reverseAnyList AnyNil = AnyNil 

-- nope - the caller decides the type of a here which is a BIG NO THANKS
{-
filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList _ AnyNil = AnyNil
filterAnyList f (AnyCons a as) = if f a
                               then (AnyCons a (filterAnyList f as))
                               else filterAnyList f as
-}

-- sure
countAnyList :: AnyList -> Int
countAnyList (AnyCons a as) = 1 + countAnyList as
countAnyList AnyNil = 0

-- nope
foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

-- yes
isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _      = False

-- nope - we know nothing about this thing so showing it is a NO WAY
instance Show AnyList where
  show = error "What about me?"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- Input! It does not appear on the right hand side. The only thing we can do is plop it through the provided function

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance (Eq a) => Eq (TransformableTo a) where
    (TransformWith f a) == (TransformWith g b) = f a == g b 

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?
--
-- I'd like to write this but I have no guarantee the output type of f will be of type input
{-
instance Functor TransformableTo where
    fmap f (TransformWith g a) = TransformWith (g . f) a
-}


{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

equals :: EqPair -> Bool
equals (EqPair a b) = a == b

notEquals :: EqPair -> Bool
notEquals = not . equals

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair2 a where
    EqPair2 :: Eq a => a -> a -> EqPair2 a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- No, we'd lose the Eq constraint


{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox i _)) = i

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox                                      = 1
countLayers (IntBox _ _)                                  = 2
countLayers (StringBox _ (IntBox _ _))                    = 3
countLayers (BoolBox _ (StringBox _ (IntBox _ EmptyBox))) = 4

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- We can't have MysteryBox a because we can't constraint 'a' to be One Of The Existing Ones Thanks

{-
removeLayer :: MysteryBox a -> MysteryBox a
removeLayer EmptyBox         = EmptyBox
removeLayer (IntBox _ a)     = a
-- removeLayer (StringBox _ a)  = a
-- removeLayer (BoolBox _ a)    = a
-}


{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hListHead :: HList (a, b) -> a
hListHead (HCons head _) = head

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList ((Int, String, Bool, ()),()) -> Int
patternMatchMe (HCons (a, _, _, _) _) = a

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- not today, daddio

{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
    HTEmpty :: HTree Empty
    HTCons  :: l -> a -> r -> HTree (Branch l a r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch l a r) -> HTree (Branch (HTree Empty) a r)
deleteLeft (HTCons _ a r) = HTCons HTEmpty a r 

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. Recursion is your friend here - you
-- shouldn't need to add a constraint to the GADT!

instance (Eq l, Eq a, Eq r) => Eq (HTree (Branch l a r)) where
    (HTCons l a r) == (HTCons l' a' r') 
      = (l == l') && (a == a') && (r == r')

instance Eq (HTree Empty) where
    HTEmpty == HTEmpty = True

{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
f :: AlternatingList Bool Int
f = ACons True (ACons 1 (ACons False (ACons 2 (ACons True ANil))))

data AlternatingList a b where
  ANil  :: AlternatingList a b 
  ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts (ACons a (ACons _ as)) = [a] ++ getFirsts as
getFirsts (ACons a ANil) = [a]
getFirsts ANil = []

getSeconds :: AlternatingList a b -> [b]
getSeconds (ACons _ (ACons b bs)) = [b] ++ getSeconds bs
getSeconds (ACons _ ANil) = []
getSeconds (ANil) = []

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues list = (mconcat (getFirsts list), mconcat (getSeconds list))

foldValues2 :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues2 list = foldyBoy (mempty, mempty) list

foldyBoy :: (Monoid a, Monoid b) => (a, b) -> AlternatingList a b -> (a, b)
foldyBoy x ANil = x
foldyBoy (as, bs) (ACons a next)
    = swap (foldyBoy (bs, a <> as) next)
        
{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals x y) = eval x == eval y
eval (Add a b)    = eval a + eval b
eval (If i a b) = if eval i then eval a else eval b
eval (IntValue i)  = i
eval (BoolValue b) = b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

dirtyParse :: DirtyExpr -> Maybe (Expr a)
-- dirtyParse (DirtyEquals (DirtyIntValue a) (DirtyIntValue b))  = Equals <$> dirtyParse (DirtyIntValue a) <*> dirtyParse (DirtyIntValue b)
-- dirtyParse (DirtyAdd (DirtyIntValue a) (DirtyIntValue b)) 
  -- = Just $ Add (IntValue a) (IntValue b)
-- dirtyParse (DirtyIf a b c)    = If <$> dirtyParse a <*> dirtyParse b <*> dirtyParse c
dirtyParse (DirtyIntValue i)  = Just $ IntValue i
-- dirtyParse (DirtyBoolValue b) = Just $ BoolValue b

parse :: DirtyExpr -> Maybe (Expr Int)
parse = error "Implement me"

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe'?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  -- ...

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs = error "Implement me, and then celebrate!"

