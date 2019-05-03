module ClassState
where

import Data.Map (Map)
import qualified Data.Map as Map
-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq, Ord)

data ClassState = ClassState (Map (InstrType,Int) [String]) deriving (Show)
-- TODO - Trebuie definit ClassState

-- Initializez un ClasState gol care contine un Map gol
initEmptyClass :: ClassState
initEmptyClass = ClassState (Map.empty)

-- Inserez un element in Map-ul ClassState-ului, astfel incat sa fie unic (pt key)
-- cu cheia = (numarul curent de elemente, tipulIntrarii) si valoarea exact
-- [String] oferita parametru
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (ClassState map) instrType value = ClassState resultMap
    where order = Map.size map
          resultMap = (Map.insert (instrType,order) value map)

-- Mi-am definit doua functii fold si filter pentru un Map. Initial extrag cu un
-- filter elementele dorite (care au al doilea parametru al cheii egal cu tipul
-- de intrare dorita, iar apoi un map pentru a extrage doar valorile lor
-- corespunzatoare
getValues :: ClassState -> InstrType -> [[String]]
getValues myClass instrType = foldPeClasa (filterPeClasa instrType myClass)

filterPeClasa :: InstrType -> ClassState -> ClassState
filterPeClasa instrType (ClassState map) = ClassState filteredMap
    where filteredMap = (Map.filterWithKey (\k _ -> fst k == instrType) map)

foldPeClasa :: ClassState -> [[String]]
foldPeClasa (ClassState map) = Map.foldr (\pereche acc-> pereche : acc) [] map