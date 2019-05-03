module Parser
where

--import Util
import Data.Maybe
import InferenceDataType
import ClassState
-- Definire Program

data ClassWrapper = ClassWrapper { nume :: String
                                 , classState :: ClassState
                                 , parinte :: String
                                 } deriving (Show)

global = ClassWrapper "Global" initEmptyClass "Global"
-- Am creat un wrapper pentru tipul clasa care contine numele,
-- classState-ul, respectiv numele parintelui

-- Program ce contine o lista de Wrappere.
data Program = Program [ClassWrapper] deriving (Show)
-- Am definit Instructiunea ca fiind o linie de program
data Instruction = Instruction String deriving (Show)

getList :: Program -> [ClassWrapper]
getList (Program lista) = lista

getClassByName :: String -> Program -> ClassWrapper
getClassByName numeClasa (Program l1) = (head (filter (\x -> nume x == numeClasa) l1))
-- Initializez un program gol, avand lista ce contine doar clasa Global
initEmptyProgram :: Program
initEmptyProgram = Program [global]

-- Extrag cu un filter din lista de Wrappere clasa Global si ii afisez
-- variabilele din classState
getVars :: Program -> [[String]]
getVars (Program l1) = getValues (classState clasaGlobal) Var
                                 where clasaGlobal = getClassByName "Global" (Program l1)

-- Aplic map pe lista de Wrappere si inlocuiesc elementele cu numele lor
getClasses :: Program -> [String]
getClasses (Program l1) = map (\x -> nume x) l1

-- Aplic filter pe lista de Wrappere pentru a alege elementul dorit
-- iar apoi aplic functia parinte pentru a-i extrage valoarea parintelui
getParentClass :: String -> Program -> String
getParentClass numeClasa (Program l1) =  parinte clasaDorita
                                where clasaDorita = getClassByName numeClasa (Program l1)

-- 
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass numeClasa (Program l1) = if (classExists numeClasa l1) 
                                            then getValues (classState clasaDorita) Func
                                        else []
    where clasaDorita = getClassByName numeClasa (Program l1)
-- =================================================
-- Instruction poate fi ce considerați voi

-- Functie ce sterge spatiile multiple si lasa un singul spatiu dintr-un String
rem2Spaces :: String -> String
rem2Spaces [' '] = [' ']
rem2Spaces [x] = [x]
rem2Spaces [] = []
rem2Spaces (x1:x2:xs)
    | x1 == ' ' && x2 == ' ' = (rem2Spaces (x2:xs))
    | x1 /= ' ' && x2 == ' ' = x1:(rem2Spaces (x2:xs))
    | otherwise = x1:x2:(rem2Spaces xs)

-- Functie care face tokenizarea unui String dupa un delimitator cu
-- ajutorul unui acumulator
tokenize :: String -> Char -> String -> [String] -> [String]
tokenize [] delim current acc = current:acc
tokenize (x:xs) delim current acc = if x /= delim
                                        then tokenize xs delim (x:current) acc
                                    else tokenize xs delim [] (current:acc)

-- Functie finala care foloseste functia de tokenizare definita anterior
imparteInTokeni :: String -> Char -> [String]
imparteInTokeni string delim = map (reverse) (reverse (tokenize string delim [] []))

-- Functie ce foloseste functia definita anterior pentru tokenizarea inputului
parse :: String -> [Instruction]
parse continutFisier = map (Instruction) (filter (\x->x /= "") content)
    where content = (imparteInTokeni (rem2Spaces continutFisier) '\n')

-- Functie ce imi returneaza tipul instructiunii citite (Variabila,Clasa,etc.)
whatKind :: Instruction -> String
whatKind (Instruction instruction)
    | (imparteInTokeni instruction ' ')!!0 == "class" = "Clasa"
    | (imparteInTokeni instruction ' ')!!0 == "newvar" = "Variabila"
    | otherwise = "Functie"

-- Functie ce imi formeaza lista de Stringuri specifica Variabilei
variableListForm :: String -> [String]
variableListForm instruction = map (eliminateSpaces) ((head imparteDupaSpatiu):
                                                     (head (tail imparteDupaEgal)):[])
    where imparteDupaEgal = (imparteInTokeni instruction '=')
          imparteDupaSpatiu = (tail (imparteInTokeni (head imparteDupaEgal) ' '))

-- Functie care extrage numele variabilei dintr-o instructiune de tip Var.
getVarName :: String -> String
getVarName instruction = eliminateSpaces (head (tail imparteDupaEgalSiSpatiu))
    where imparteDupaEgalSiSpatiu = (imparteInTokeni  imparteDupaEgal ' ')
          imparteDupaEgal = (head (imparteInTokeni instruction '='))

-- Functie care extrage numele clasei dintr-o instructiune de tip Var.
getVarClass :: String -> String
getVarClass instruction = eliminateSpaces (head imparteDupaEgal)
    where imparteDupaEgal = (tail (imparteInTokeni instruction '='))

-- Functie care returneaza valoarea booleana corespunzatoare existentei
-- clasei in Program
classExists :: String -> [ClassWrapper] -> Bool
classExists numeClasa l1 = elem numeClasa toateClasele
    where toateClasele = (getClasses (Program l1))

-- Functie care insereaza intr-o clasa dintr-un program o noua intrarea
-- in tabela Map
insertInProgram :: String -> [String] -> InstrType -> Program -> Program
insertInProgram className string instrType (Program l1) = Program (map functiamea l1)
    where functiamea = (\x -> if (nume x) == className
                                then (ClassWrapper className (functie2 x) parinte)
                             else x)
          parinte = (getParentClass className (Program l1))
          functie2 x = (insertIntoClass (classState x) instrType string)

-- Functie care creaza o clasa noua cu un classState gol si o introduce
-- in program
createClassInProgram :: String -> String -> Program -> Program
createClassInProgram className parentName (Program l1) 
    |classExists parentName l1 = Program (clasanoua:l1)
    |otherwise = Program ((ClassWrapper className initEmptyClass "Global"):l1)
    where clasanoua = (ClassWrapper className initEmptyClass parentName)

-- Functie care returneaza valoarea de retur a unei functii data printr-o
-- instructiune corespunzatoare.
getFunctionReturnType :: String -> String
getFunctionReturnType instruction = eliminateSpaces imparteDupaSpatiu
   where imparteDupaSpatiu = (head (imparteInTokeni instruction ' '))

-- Functie care returneaza clasa unei functii date printr-o instructiune corespunzatoare
getFunctionClass :: String -> String
getFunctionClass instruction = eliminateSpaces (head imparteDupaSpatiu)
   where imparteDupaDouaPuncte = (head (imparteInTokeni instruction ':'))
         imparteDupaSpatiu = ( tail (imparteInTokeni imparteDupaDouaPuncte ' '))

-- Functie care returneaza numele functiei 
getFunctionName :: String -> String
getFunctionName instruction = eliminateSpaces imparteDupaParanteza
   where imparteDupaDouaPuncte = (head(tail (tail (imparteInTokeni instruction ':'))))
         imparteDupaParanteza = (head (imparteInTokeni imparteDupaDouaPuncte '('))

-- Functie care elimina toate spatiile dintr-un String
eliminateSpaces :: String -> String
eliminateSpaces [] = []
eliminateSpaces (x:xs)
    |(x == ' ') = eliminateSpaces xs 
    |otherwise = x:(eliminateSpaces xs)

-- Functie care elimina ultimul caracter daca este ')' sau ') '    
eliminateLastChar :: String -> String
eliminateLastChar ") " = []
eliminateLastChar [')'] = []
eliminateLastChar (x:xs) = x:eliminateLastChar xs

-- Functie care sterge ultimul spatiu dintr-un String
deleteLastSpace :: String -> String
deleteLastSpace [' '] = []
deleteLastSpace [] = []
deleteLastSpace (x:xs) = x:(deleteLastSpace xs)

-- Functie care returneaza o lista cu parametri funcitie 
getFunctionParams :: String -> [String]
getFunctionParams instruction = map (eliminateSpaces) imparteDupaVirgula
    where imparteDupaParanteza = (imparteInTokeni (eliminateLastChar instruction) '(')
          imparteDupaVirgula = (imparteInTokeni (head ( tail imparteDupaParanteza)) ',')

-- Functie care returneaza numele clasei dintr-o functie
getClassName :: String -> String
getClassName instruction = (imparteDupaSpatiu!!1)
    where imparteDupaSpatiu = (imparteInTokeni (deleteLastSpace instruction) ' ')

-- Functie care formeaza lista de input pentru o functie
functionListForm :: String -> [String]
functionListForm instruction = filter (\x->x /= "") ((nume:returnType:params))
    where nume = (getFunctionName instruction)
          returnType = (getFunctionReturnType instruction)
          params = (getFunctionParams instruction)

-- Functie care verifica daca toti parametri unei functii exista
allParamsExist :: [String]->[String]->Bool
allParamsExist [""] listaDeClase = True
allParamsExist [] listaDeClase = True
allParamsExist paramList listaDeClase = existaElement && (allParamsExist (tail paramList) listaDeClase)
    where existaElement = (elem (head paramList) listaDeClase)

-- FUNCTIA DE INTERPRET 
interpret :: Instruction -> Program -> Program
interpret (Instruction ins) (Program listaDeClase)
    |tip == "Variabila" = if clasaExista
        then insertInProgram "Global" formatVar Var (Program listaDeClase)
        else (Program listaDeClase)
    |tip == "Clasa" = if (numberWords == 2)
        then (if (classExists (getClassName ins) listaDeClase == False)
                 then clasanoua
              else (Program listaDeClase))
        else (if (classExists (getClassName ins) listaDeClase == False)
                then (createClassInProgram (getClassName ins) parentName (Program listaDeClase))
              else (Program listaDeClase))
    |otherwise = if clasFuncExista && (allParamsExist paramFunctie listaClase) &&
                     (elem functionRetType listaClase) 
                    then insertInProgram functionClass functionFormat Func (Program listaDeClase)
                else (Program listaDeClase)
    where tip = whatKind (Instruction (deleteLastSpace ins))
          clasaExista = (classExists (getVarClass (deleteLastSpace ins)) listaDeClase)
          clasFuncExista = (classExists (getFunctionClass (deleteLastSpace ins)) listaDeClase)
          formatVar = (variableListForm (deleteLastSpace ins))
          numberWords = (length (imparteInTokeni (deleteLastSpace ins) ' '))
          clasanoua = (createClassInProgram (getClassName ins) "Global" (Program listaDeClase))
          parentName = ((imparteInTokeni (deleteLastSpace ins) ' ')!!3)
          paramFunctie = (getFunctionParams (deleteLastSpace ins))
          listaClase = (getClasses (Program listaDeClase))
          functionRetType = (getFunctionReturnType (deleteLastSpace ins))
          functionClass = (getFunctionClass (deleteLastSpace ins))
          functionFormat = (functionListForm (deleteLastSpace ins))

-- Functie care returneaza clasa din care face parte o variabila
getClassOfVariable :: String -> Program -> String
getClassOfVariable numeVar (Program listaDeClase) 
    |((length aparitie) > 0) = ((head.tail.head) aparitie)
    |otherwise = "Nothing"
    where aparitie = (filter (\x-> (head x) == numeVar) (getVars (Program listaDeClase)))

-- Functie care returneaza toate functiile cu numele dat din arborele de mosteniri
-- ale unei variabile
functiiDisponibile :: String -> String -> Program -> [[String]]
functiiDisponibile numeF "Global" program = (filtruPeNume (getFuncsForClass "Global" program))
    where filtruPeNume = filter (\x->head x == numeF)
functiiDisponibile numeF numeC program = (filtruPeNume functiiClasa) ++
                                         (filtruPeNume (functiiDisponibile numeF parinte program))
    where filtruPeNume = filter (\x->head x == numeF)
          functiiClasa = (getFuncsForClass numeC program)
          parinte = (getParentClass numeC program)

-- Functie care returneaza valoarea unui Maybe -> String
getValueOfMaybe :: Maybe String -> String
getValueOfMaybe Nothing = "Nothing"
getValueOfMaybe (Just x) = x

-- Functie care verifica daca toate elementele din lista de Maybe String
-- Corespund listei de parametri ale functiei
verifica :: [Maybe String] -> [String] -> String -> Maybe String
verifica [] [] valReturFunctie = Just valReturFunctie
verifica arg [] valReturFunctie = Nothing
verifica [] paramF valReturFunctie= Nothing
verifica arg paramF valReturFunctie 
   |elementLista1 == elementLista2 = verifica (tail arg) (tail paramF) valReturFunctie
   |otherwise = Nothing
   where elementLista1 = (getValueOfMaybe (head arg))
         elementLista2 = (head paramF)

-- Functie care returneaza valoarea pe care s-a facut match din lista de parametri
-- si argumente ale functiei
getResult :: [Maybe String] -> [[String]] -> Maybe String
getResult arg [] = Nothing
getResult arg listaDeFunctii
    |rezultat /= Nothing = rezultat
    |otherwise = getResult arg (tail listaDeFunctii)
    where rezultat = verifica arg (drop 2 (head listaDeFunctii)) (head (drop 1 (head listaDeFunctii)))

-- FUNCTIA DE INFERENTA
infer :: Expr -> Program -> Maybe String
infer (Va varName) (Program listaDeClase)
    | answer /= "Nothing" = Just answer
    |otherwise = Nothing
    where answer = (getClassOfVariable varName (Program listaDeClase))

infer (FCall numeVar numeFunctie listaArgumente) program 
    |(getClassOfVariable numeVar program /= "Nothing") =
        if (length listaFunctiiDisponibile > 0)
            then if (elem Nothing (map (\x -> infer x program) listaArgumente))
                then Nothing
            else getResult (map (\x -> infer x program) listaArgumente) listaFunctiiDisponibile
        else Nothing
    |otherwise = Nothing
    where listaFunctiiDisponibile = functiiDisponibile numeFunctie clasaVar program
          clasaVar = (getClassOfVariable numeVar program)

