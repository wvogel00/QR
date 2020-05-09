module CFParser where
    -- parser for chemical formula

import Text.Trifecta
import Data.List
import Data.Maybe
import Control.Applicative
import Type


parseCF :: Parser [(Maybe Element, Int)]
parseCF = (:) <$> element <*> many element

element :: Parser (Maybe Element, Int)
element = (tuple <$> upper <*> try (many lower) <*> try (many digit)) where
    tuple u l n = (findElem $ u:l, safeRead n)
    safeRead "" = 1
    safeRead x = read x

findElem :: String -> Maybe Element
findElem elem = findElem $ filter ((==elem).show) [Na ..] where
    findElem [] = Nothing
    findElem [a] = Just a