{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import           Data.Either     (Either, either)
import           Data.Map        (Map)
import qualified Data.Map as Map (delete, insert, lookup)
import           Data.Set        (Set)
import qualified Data.Set as Set (delete, insert, member)
import           Data.Maybe      (Maybe, maybe)

data Address =
  Address
    Int -- Street number
    String -- Street name
  deriving (Eq, Show)

modifyStreetNumberOld :: (Int -> Int) -> Address -> Address
modifyStreetNumberOld fn (Address num name) = Address (fn num) name

moveNextDoor = modifyStreetNumberOld (+1)

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

--streetNumber :: Functor f => (Int -> f Int) -> Address -> f Address
streetNumber :: Lens' Address Int
streetNumber fn (Address num name) = flip Address name <$> fn num

modifyStreetNumber' :: (Int -> Int) -> Address -> Address
modifyStreetNumber' fn = runIdentity . streetNumber (Identity . fn)

data Person =
  Person
    String -- Name
    Address
  deriving (Eq, Show)

--address :: Functor f => (Address -> f Address) -> Person -> f Person
address :: Lens' Person Address
address fn (Person name addr) = Person name <$> fn addr

modifyPersonAddress' :: (Address -> Address) -> Person -> Person
modifyPersonAddress' fn = runIdentity . address (Identity . fn)

-- This is 'over' from the Lens library
modify :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
modify m f = runIdentity . m (Identity . f)

modifyStreetNumber'' :: (Int -> Int) -> Address -> Address
modifyStreetNumber'' = modify streetNumber

modifyPersonAddress'' :: (Address -> Address) -> Person -> Person
modifyPersonAddress'' = modify address

--personStreetNumber :: Functor f => (Int -> f Int) -> Person -> f Person
personStreetNumber :: Lens' Person Int
personStreetNumber = address . streetNumber

setPersonStreetNumber :: Int -> Person -> Person
setPersonStreetNumber = modify personStreetNumber . const

newtype Const a b = Const { runConst :: a }
  deriving (Eq, Show)

instance Functor (Const a) where
  fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  Const a <*> Const b = Const (a `mappend` b)

getPersonAddress :: Person -> Address
getPersonAddress = runConst . address Const

getAddressStreetNumber :: Address -> Int
getAddressStreetNumber = runConst . streetNumber Const

get :: ((a -> Const a b) -> s -> Const a t) -> s -> a
get k = runConst . k Const

set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set k = modify k . const

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- set . get == id
getSetLaw :: Eq s => Lens' s a -> s -> Bool
getSetLaw l s = set l (get l s) s == s

-- get . set == id
setGetLaw :: Eq a => Lens' s a -> s -> a -> Bool
setGetLaw l s a = get l (set l a s) == a

-- set . set == set
setSetLaw :: Eq s => Lens' s a -> s -> a -> Bool
setSetLaw l s a = set l a (set l a s) == set l a s

-- Look up key 'k' in a map 'm' and have a way to delete it (if the provided
-- (a -> f b) function returns a Nothing), or update it (if it returns a
-- (Maybe b) value)
at :: Ord k => k -> Lens' (Map k v) (Maybe v)
at k f m = fmap updateMap . f . Map.lookup k $ m
  where
    updateMap = maybe
                  (Map.delete k m)
                  (\v -> Map.insert k v m)

-- HW: contains :: Ord a => a -> Lens' (Set a) Bool
-- Similar to 'at' but for Set deletion / addition
contains :: Ord a => a -> Lens' (Set a) Bool
contains v f s = fmap updateSet . f . Set.member v $ s
  where
    updateSet b = if b then Set.delete v s else Set.insert v s

data Company =
  Company
    Person -- CEO
    Person -- CTO
    Person -- CFO
    [Person] -- Employees
    String -- Name
  deriving (Eq, Show)

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

--companyPersons :: Applicative f => (Person -> f Person) -> Company -> f Company
companyPersons :: Traversal' Company Person
companyPersons f (Company ceo cto cfo emps name) =
  Company <$> f ceo <*> f cto <*> f cfo <*> traverse f emps <*> pure name

moveAllPersonsNextDoor :: Company -> Company
moveAllPersonsNextDoor = modify (companyPersons . address . streetNumber) (+1)

class Profunctor f where
  dimap :: (b -> a) -> (c -> d) -> f a c -> f b d

instance Profunctor (->) where
  dimap b2a c2d a2c = c2d . a2c . b2a

-- For Isomorphic types, like newtypes
--type Iso s t a b = forall p. Profunctor p => p a b -> p s t
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

-- Kleisli is similar but on Monad
newtype Star f a b = Star (a -> f b)

instance Functor f => Profunctor (Star f) where
  dimap b2a c2d (Star a2fc) = Star (fmap c2d . a2fc . b2a)

-- HW: lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b

iso :: (s -> a) -> (b -> t) -> Iso s t a b
--iso s2a b2t pafb = dimap s2a (fmap b2t) pafb
iso s2a b2t = dimap s2a (fmap b2t)

-- HW: unget :: Iso s t a b -> b -> t

class Profunctor p => Strong p where
  first :: p a b -> p (a, c) (b, c)

instance Functor f => Strong (Star f) where
  first (Star a2fb)= Star $ \(a, c) -> (, c) <$> a2fb a

-- type Lens s t a b = forall p. Strong p => p a b -> p s t

class Profunctor p => Choice p where
  left :: p a b -> p (Either a c) (Either b c)

instance Choice (->) where
  left a2b = \case
    Left a  -> Left . a2b $ a
    Right c -> Right c

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either a t) -> Prism s t a b
-- prism b2t seat pafb = dimap seat (either (fmap b2t) pure) . left $ pafb
prism b2t seat = dimap seat (either (fmap b2t) pure) . left

_Left :: Prism' (Either a b) a
_Left = prism Left $ \case
  Left v  -> Left v
  Right v -> Right $ Right v

_Right :: Prism' (Either a b) b
_Right = prism Right $ \case
  Right v -> Left v
  Left v  -> Right $ Left v

-- (#) as a smart constructor

data Json
  = Num Double
  | Str String
  | Array [Json]
  | Object (Map String Json)
  | Boolean Bool
  | Null
  deriving (Eq, Show)

_Boolean :: Prism' Json Bool
_Boolean = prism Boolean $ \case
  Boolean b -> Left b
  v         -> Right v

_Object :: Prism' Json (Map String Json)
_Object = prism Object $ \case
  Object o -> Left o
  v        -> Right v

-- preview :: Prism s t a b -> b -> t

-- project :: Prism s t a b -> s -> Either t a
-- embed :: Prism s t a b -> b -> t

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

-- HW: Instance for Predicate a = Predicate (a -> Bool) and Const

-- type Fold s a = forall f. (Applicative f, Contravariant f) => (a -> f a) -> s -> f s

--traverse :: Applicative f => (a -> f b) -> [a] -> [b]
--traverse :: Traversal [a] [b] a b

both :: Traversal (a, a) (b, b) a b
both a2b (a1, a2) = (,) <$> a2b a1 <*> a2b a2

newtype Store a b = Store (a -> b, a)

-- instance Functor (Store a)
-- instance Comonad (Store a)
