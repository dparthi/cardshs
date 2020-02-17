module Permutation where

import Data.List (delete)

import Utils (factorial)

numberOfPermutations :: [a] -> Integer
numberOfPermutations dataSet = factorial (toInteger (length dataSet))

numberOfMembersPerGroup :: [a] -> Integer
numberOfMembersPerGroup dataSet = factorial (toInteger ((length dataSet) - 1))

getPermutationAtIndex :: Eq a => [a] -> Integer -> [a]
getPermutationAtIndex dataSet 0 = dataSet
getPermutationAtIndex dataSet index = firstElement : (getPermutationAtIndex (delete firstElement dataSet) (mod index (numberOfMembersPerGroup dataSet)))
    where firstElement = dataSet !! fromIntegral (quot index (numberOfMembersPerGroup dataSet))
