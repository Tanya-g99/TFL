module Algo where

import Grammar
import EarlyAlgorithm

data Algo = Algo { grammar :: Maybe Grammar }

--?? 
fit :: Algo -> Grammar -> Algo
fit algo grammar = algo { grammar = Just grammar }

predict :: Algo -> String -> Bool
predict algo word = predictWord (fromJust (grammar algo)) word