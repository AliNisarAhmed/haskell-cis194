module TowerOfHanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a _ c = [(a, c)]
hanoi n a b c = hanoi (n - 1) a b c ++ hanoi 1 a c b ++ hanoi (n - 1) b a c