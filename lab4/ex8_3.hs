module Dequeue
 ( Dequeue
 , emptyDEQ     -- :: Dequeue a
 , isEmptyDEQ   -- :: Dequeue a -> Bool
 , lengthDEQ    -- :: Dequeue a -> Int, O(1)
 , firstDEQ     -- :: Dequeue a -> Maybe a,  O(1)
 , lastDEQ      -- :: Dequeue a -> Maybe a, O(1)
 , takeFrontDEQ -- :: Int -> Dequeue a -> [a], O(n)
 , takeBackDEQ  -- :: Int -> Dequeue a -> [a], O(n)
 , pushFrontDEQ -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popFrontDEQ  -- :: Dequeue a -> Maybe (a, Dequeue a), O(1) amortised
 , pushBackDEQ  -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popBackDEQ   -- :: Dequeue a -> Maybe (a, Dequeue a), O(1) amortised
 , fromListDEQ  -- :: [a] -> Dequeue a, O(n)
 ) where
 
emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> a
lastDEQ :: Dequeue a -> a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> (a, Dequeue a)
pushBackDEQ :: Dequeue a -> a -> Dequeue a
popBackDEQ :: Dequeue a -> (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a


newtype Dequeue a = MkDequeue [a] deriving Show

emptyDEQ = MkDequeue []
isEmptyDEQ (MkDequeue s) = null s
lengthDEQ (MkDequeue s) = length s
firstDEQ (MkDequeue (s:ss)) = s
lastDEQ (MkDequeue s) = (last s)
takeFrontDEQ a (MkDequeue s) = take a s
takeBackDEQ a (MkDequeue s) = drop (length s - a) s
pushFrontDEQ (MkDequeue s) a = (MkDequeue ([a]++s))
popFrontDEQ (MkDequeue (s:ss)) = (s, MkDequeue ss)
pushBackDEQ (MkDequeue s) a = (MkDequeue (s++[a]))
popBackDEQ (MkDequeue s) = 	(last s, MkDequeue (init s))
fromListDEQ a = (MkDequeue a) 