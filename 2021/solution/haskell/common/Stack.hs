module Stack (module Stack) where

-- do not derive Show since you define it
newtype Stack a = Stack [a] deriving (Eq, Ord)

-- there was a redundant pattern matching here
printelems :: (Show a) => [a] -> String
printelems [] = ""
printelems (x : xs) = if null xs then show x else show x ++ "->" ++ printelems xs

instance (Show a) => Show (Stack a) where
  show (Stack l) = printelems l

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

push :: a -> Stack a -> Stack a
push x (Stack s) = Stack (x : s)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (h : s)) = (Just h, Stack s)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (h : s)) = Just h
