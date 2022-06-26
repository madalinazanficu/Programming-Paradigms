{- Varianta b - 2020 -}
type FreqTable = [(Char, Int)]

rareChars::FreqTable -> Int -> [Char]
rareChars freq n = foldl (\acc per -> if snd per < n then
                                      (fst per):acc
                                      else
                                        acc) [] freq

insChar::Char -> FreqTable -> FreqTable
insChar car freq = case lookup car freq of
                  Nothing -> (car, 1):freq
                  Just frecv -> (car, frecv + 1): (filter (\per -> (fst per) /= car) freq)


textToStr :: String -> String
textToStr = concat . nub . words -- nub elimina duplicatele
                                -- duplicatele se pot elimina si cu un fold
                                -- words desparte in string in cuvinte
                                -- concat = concateneaza toate cuvintele

-- se face tabla de freceventa pentru toate caracterele folosind un foldr si functia de insertie creata anterior                               
textToTable :: String -> FreqTable
textToTable = foldr insChar [] . textToStr                                


{- Varianta a - 2020 -}
type Text = String
type Pair = (String, Int)
type Index = [(String, [Int])]


textToLines::Text->[(Int, Text)]
textToLines text = let linii = textToLines text
                      bune = map (\line -> filter (\x -> x /= '!' && x /= '?' && x /= '.' && x /= ',')  line) linii
                      perechi = foldl (\acc x -> (length acc + 1, x): acc) [] bune
                      in reverse(perechi)
                      

insertPairs::(Text, Int) -> Index -> Index
insertPairs (cuvant, index) lista = let new_lista = map(\per -> if (first per) == cuvant then
                                                                  if (elem index (second per) == True) then
                                                                      per
                                                                  else
                                                                      (cuvant, index:(second per))
                                                                else
                                                                  per
                                                                ) lista in
                                    case lookup cuvant lista of
                                      Nothing -> (cuvant, index):lista
                                      Just lista -> new_lista

allPairs :: Text -> [Pair]
allPairs txt = [ (word, line) | (line, text) <- textToLines txt, word <- words text]



{-RECAPITULARE CLASE SI INSTANTE-}
{-1.-}


data Card1 = C22 | C33 | C44  | C55 | C66 | C77 | C88 | C99 | C110 | J1 | Q1 | K1|  A1 deriving Show
data Value1 = Only1 Int | Either1 Int Int deriving Show

{-a este template din CPP-}
class Valuable a where
  value::a -> Value1

instance (Valuable Card1) where
    value C22 = Only1 2
    value C33 = Only1 3
    value C44 = Only1 4
    value C55 = Only1 5
    value C66 = Only1 6
    value C77 = Only1 7
    value C88 = Only1 8
    value C99 = Only1 9
    value C110 = Only1 10
    value J1 = Only1 10
    value Q1 = Only1 10
    value K1 = Only1 10
    value A1 = Either1 1 11
    
correct_value:: Value1 -> Maybe Value1
correct_value (Only1 valoare) = if valoare <= 21 then
                                  Just (Only1 valoare)
                                else
                                  Nothing 
correct_value (Either1 val1 val2) = if (val1 <= 21) && (val2 <= 21) then
                                      Just (Either1 val1 val2)
                                    else
                                      if (val1 <= 21) then
                                        Just (Only1 val1)
                                      else
                                        if (val2 <= 21) then
                                          Just (Only1 val2)
                                        else
                                          Nothing
                                  
v::Value1
v = Only1 22
{- correct_value v =>>>>  Nothing  -}
v1::Value1
v1 = Either1 17 22
{- correct_value v1 =>>>> Just (Only1 17)-}

handValue::[Card] -> Value1
handValue cards = if (number_A >= 1) then
                      Just (Either1 (sum_without + number_A) (sum_without + number_A + 10))
                    else
                      Just (Only1 sum_without) where
                    sum_without = foldl(\acc x -> acc + x) 0 [v | c <- cards, let Only1 v = value c]
                    number_A = length $ filter (\x -> x == A1) cards

{-POINT FREE-}
correctHandValue :: [Card] -> Maybe Value
correctHandValue = correctValue . handValue



{- RECAPITULARE TIPURI DE DATE UTILIZATOR-}

{-1. tip TYPEDEF-}
type Point = (Int, Int)
p0::Point
p0 = (1, 2)
{- :t p0 =>> p0::Point -}
p1 = (2, 3)


{-2. constructia DATA-}
{-Point3D este constructorul de tip-}
{-MyPoint este constructorul de date-}
data Point3D = MyPoint Double Double Double deriving Show
p2 = MyPoint 1.2 1.4 1.3 

{-:t p2 =>> p2::Point3D-}
{- daca scriu p2 in consola => MyPoint 1.2 1.4 1.3  -}


{-3. TIP ENUMERAT-}
data Card = C2 Int | C3 | C4  | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | A deriving Show
data Value = Only Int | Either Int Int deriving Show

firstCard = C2 14
secondCard = C4

{-Pentru acest tip enumerat putem face pattern matching pe constructori-}
giveValue A = 10
giveValue J = 12
giveValue Q = 13
giveValue (C2 x) = x
giveValue _ = 1

{- giveValue firstCard => extrage 12.1 -}
{- giveValue secondCard => 1 din pattern matching pe _ -}





{- 4.TIP INREGISTRAT-}
data New_Point3D = MyNew_Point3D
                  {
                    px::Double,
                    py::Double,
                    pz::Double
                  } deriving Show

point::New_Point3D
point = MyNew_Point3D {px = 11, py = 12, pz = 13}

{-pattern matching pentru extragerea valorilor-}
{-sau pentru a accesa campul => in consola apelez px point-}
getX (MyNew_Point3D x _ _) = x
getY (MyNew_Point3D _ y _) = y
getZ (MyNew_Point3D _ _ z) = z


reverse_coord :: New_Point3D -> p -> New_Point3D
reverse_coord point3D@(MyNew_Point3D px py pz) how = case how of
                                                  reversed -> MyNew_Point3D py pz px
                                                  otherwise -> point3D

{-reverse_coord point "reversed" =>> MyNew_Point3D {px = 12.0, py = 13.0, pz = 11.0}   -}

data Trie = Root
            {
              children::[Trie]
            }
            | Node
            {
              myChar::Char,
              children::[Trie]
            } deriving Show

myTrie::Trie
myTrie = Root []

leftChild::Trie
leftChild = Node 'a' []

rightChild::Trie
rightChild = Node 'b' []

{-Intrebare: Cum actualizez children din myTrie?
  Pot actualiza doar intr-o alta variabila
-}
secondTrie::Trie
secondTrie = myTrie {children = [leftChild, rightChild]}






{-5. TIPUL PARAMETRIZAT-}
data Maybe a = Just a | Nothing deriving (Show, Eq, Ord)
maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead _ = Nothing 

Laborator


anInteger = 42
aFloat = 42.2
aBool = False
aFunction x = x
anotherFunction x = x + 3
yetAnotherFunction x y = min x y
theLastFunction = yetAnotherFunction 3

length1 list = if list == [] then 0 else 1 + length1 (tail list)

length2 list = case list of
  [] -> 0
  _:xs -> 1 + length2 xs

length3 [] = 0
length3 (_:xs) = 1 + length3 xs

{-
Alte funcții simple pe liste.
-}
sumList [] = 0
sumList (x:xs) = x + sumList xs

productList [] = 1
productList (x:xs) = x * productList xs

maxList [x] = x -- nu putem avea valoare pentru lista vidă
maxList (x:xs) = max x (maxList xs)

lengthFold l = foldl (\x y -> x + 1) 0 l
sumListFold l = foldl (+) 0 l
productListFold l = foldl (*) 0 l
maxListFold list = foldl max (head list) (tail list)


