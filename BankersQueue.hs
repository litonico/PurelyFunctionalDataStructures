module BankersQueue where
    import Prelude hiding (head, tail)

    data BankersQueue a = BankersQueue Int [a] Int [a]

    check :: Int -> [a] -> Int -> [a] -> BankersQueue a
    check lenf f lenr r
      | lenr <= lenf = BankersQueue lenf f lenr r
      | otherwise    = BankersQueue (lenf+lenr) (f ++ reverse r) 0 []

    snoc :: BankersQueue a -> a -> BankersQueue a
    snoc (BankersQueue lenf f lenr r) x = check lenf f (lenr + 1) (x:r)

    head :: BankersQueue a -> a
    head (BankersQueue _ [] _ _) = error "Empty"
    head (BankersQueue _ (x:_) _ _) = x

    tail :: BankersQueue a -> BankersQueue at
    tail (BankersQueue _ [] _ _) = error "Empty"
    tail (BankersQueue lenf (_:f) lenr r) = check (lenf - 1) f lenr r
