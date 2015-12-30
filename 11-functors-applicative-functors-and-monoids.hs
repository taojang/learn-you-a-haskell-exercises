import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
  fmap f Empty       = Empty
  fmap f (Value x xs) = Value (f x) (fmap f xs)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists (Value x xs) b@(Value _ _) = Value x $ combineLists xs b
combineLists a            Empty         = a
combineLists Empty        b             = b

-- Make our list a Monoid
instance Monoid (List a) where
  mempty = Empty
  (Value x xs) `mappend` b@(Value _ _) = Value x $ mappend xs b
  _            `mappend` _             = Empty

-- Make our list an Applicative
instance Applicative List where
  pure = flip Value $ Empty
  (Value f fs) <*> b@(Value x xs) = (fmap f b) `combineLists` (fs <*> b)
  _            <*> _              = Empty

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
blahValueList = Value 1 $ Value 2  $ Value 3 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2) <$> twoValueList

-- Use <$> and <*> on the lists with a binary function
multiplyAll = (*) <$> twoValueList <*> blahValueList

-- Create some lists of binary functions
binfList = Value (+) $ Value (-) $ Value (*) $ Value (/) Empty

-- Use <*> on the binary functions list and the number lists
blah = binfList <*> twoValueList <*> blahValueList
