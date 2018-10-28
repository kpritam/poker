module Poker where

import Data.List

data Rank = Ace | Two | Three | Four | Five | Six | Seven
              | Eight | Nine | Ten | Jack | Queen | King
              deriving (Eq, Ord, Bounded, Enum, Show, Read)

data Suite = Spades | Hearts | Clubs | Diamonds
              deriving (Eq, Enum, Show, Read)

data Card = Card Rank Suite deriving (Eq, Show, Read)

type Hand = [Card]

allPairs f [] = True
allPairs f [x] = True
allPairs f (x:y:ys) = f x y && allPairs f (y:ys)

setCount n k hand = let sets = groupBy (\(Card r1 _) (Card r2 _) -> r1 == r2) hand
                    in (length sets == n) && (maximum (map length sets) == k)

pair = setCount 4 2

twoPair = setCount 3 2

threeOfAKind = setCount 3 3

straight = allPairs (\(Card r1 _) (Card r2 _) -> r1 == pred r2)

flush = allPairs (\(Card _ s1) (Card _ s2) -> s1 == s2)

fullHouse = setCount 2 3

fourOfAKind = setCount 2 4

straightFlush hand = straight hand && flush hand