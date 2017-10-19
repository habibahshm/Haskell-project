import FurnitureResources

getexamined :: ([Char],[[([Char],[Char],Int)]]) -> [Char]
getexamined (a,_) = a

getlist :: ([Char],[[([Char],[Char],Int)]]) -> [[([Char],[Char],Int)]]
getlist (_,l) = l

getBelowList :: ([Char],[[([Char],[Char],Int)]]) -> [([Char],[Char],Int)]
getBelowList (_,l) = l !! 1

getRightList :: ([Char],[[([Char],[Char],Int)]]) -> [([Char],[Char],Int)]
getRightList (_,l) = l !! 0

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) = if(x==y) then ys else [y] ++ delete x ys

-- replace dyh sebtaha zy ma hya 3shan dyh bnsta5dymha fe findFurnitureUpdateHelper1 nshyl l object pair ([Char],[[([Char],[Char],Int)]]) kolo w n7ot l gdyd mkano
replace :: Eq a=> a -> a -> [a] -> [a]
replace deletant replacement l = (delete deletant l) ++ [replacement]

-- replace1 dyh hya ally bnsta5dymha fe findFurnitureUpdateHelper2 lma nygy nd5l [([Char],[Char],Int)] fl list l right aw l below bta3t l object 
replace1 :: ([Char],[Char],Int) -> ([Char],[Char],Int) -> [([Char],[Char],Int)] -> [([Char],[Char],Int)]
replace1 deletant replacement l = insertsort  replacement (delete deletant l)

-- 7awly keda t3mly l types bta3 l function dyh w replace1 w 2b2y olyly 
insertsort :: ([Char],[Char],Int) -> [([Char],[Char],Int)] -> [([Char],[Char],Int)]
insertsort element [] = [element]
insertsort element (x:xs) 
                        | (getf element) >= (getf x) = element : x : xs
                        | otherwise = x : (insertsort element xs)

data Pair = P ([Char],[Char]) deriving Eq 
getpair :: ([Char],[Char],Int) -> Pair
getpair (b,c,_) = P (b,c)
getb :: ([Char],[Char],Int) -> [Char]
getb (b,_,_) = b
getc :: ([Char],[Char],Int) -> [Char]
getc (_,c,_) = c
getf :: ([Char],[Char],Int) ->Int
getf (_,_,f) = f

findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]
findFurnitureUpdate a b c currentstatis = findFurnitureUpdateHelper1 a b c currentstatis 0 

findFurnitureUpdateHelper1 :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])] -> Int -> [([Char],[[([Char],[Char],Int)]])]
findFurnitureUpdateHelper1 a b c currentstatis idx | (idx == (length currentstatis)) = if(c=="right") then currentstatis ++ [(a,[[(b,c,1)],[]])] else currentstatis ++ [(a,[[],[(b,c,1)]])]
                                                   | ( getexamined(currentstatis !! idx) == a ) = replace (currentstatis !! idx) (a,transtion (currentstatis !! idx) b c) currentstatis
                                                   | otherwise = findFurnitureUpdateHelper1 a b c currentstatis (idx+1) 

transtion ::  ([Char],[[([Char],[Char],Int)]]) -> [Char]-> [Char] -> [[([Char],[Char],Int)]]
transtion element b c | (c == "right") = [findFurnitureUpdateHelper2 b c (getRightList element) 0,getBelowList element]
                      | otherwise = [getRightList element,findFurnitureUpdateHelper2 b c (getBelowList element) 0]

findFurnitureUpdateHelper2 :: [Char]-> [Char] -> [([Char],[Char],Int)] ->Int -> [([Char],[Char],Int)]
findFurnitureUpdateHelper2 b c listofa idx | (idx == (length listofa)) =  listofa ++ [(b,c,1)]
                                           | (getpair (listofa !! idx) == P (b,c)) = replace1 (listofa !! idx) (b,c,(getf (listofa !! idx)+1)) listofa 
                                           | otherwise =  findFurnitureUpdateHelper2 b c listofa (idx+1)
                                           
generate :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] ->[([Char],[[([Char],[Char],Int)]])]
generate room currentstatis =  generateHelper 0 0 (length room) room currentstatis

generateHelper :: Int -> Int -> Int -> [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]
generateHelper i j max room currentstatis  | (i== (max-1) && j==(max-1) )= currentstatis
                                           | i==(max-1) = findFurnitureUpdate ((room !! i) !! j) ((room !! i) !! (j+1)) "right" (generateHelper (i) (j+1) max room currentstatis)
                                           | j==(max-1) = findFurnitureUpdate ((room !! i) !! j) ((room !! (i+1)) !! j) "below" (generateHelper (i+1) 0 max room currentstatis)
                                           | otherwise = findFurnitureUpdate ((room !! i) !! j) ((room !! i) !! (j+1)) "right" (findFurnitureUpdate ((room !! i) !! j) ((room !! (i+1)) !! j) "below" (generateHelper (i) (j+1) max room currentstatis))
             
statsListHelper:: [[[[Char]]]] -> [([Char],[[([Char],[Char],Int)]])] ->  [([Char],[[([Char],[Char],Int)]])]
statsListHelper [] currentstatis = []
statsListHelper (r:rs) currentstatis = generate r (statsListHelper rs currentstatis)

statsList :: [([Char],[[([Char],[Char],Int)]])] 
statsList = statsListHelper training []

     ----         %%%%%%%%%%%%%%%%       ----------------------------------------------------------------------------------------  %%%%%%%%%%%%%%%   ------


getPossibleNeighbour :: [[([Char],[Char],Int)]] -> [[([Char],[Char],Int)]] -> [Char]
getPossibleNeighbour left top = a !! (randomZeroToX ((length a)-1)) where a = getHelper ((left !! 0) ++ (top !! 1))

getHelper :: [([Char],[Char],Int)] -> [[Char]]
getHelper [] =[] 
getHelper (x:xs) | ((getf x) ==0) = getHelper xs
                 | otherwise = [getb x] ++ getHelper ((getb x, getc x , ((getf x)-1)):xs)

getFurnStat :: [Char] -> [[([Char],[Char],Int)]]
getFurnStat a = getFurnStatHelper a (statsList)

getFurnStatHelper :: [Char] -> [([Char],[[([Char],[Char],Int)]])] -> [[([Char],[Char],Int)]]
getFurnStatHelper a (x:xs) | (getexamined x == a) = getlist x
                           | otherwise = getFurnStatHelper a xs

createEmptyRoom :: Int ->[Char]->[[[Char]]]
createEmptyRoom n a |n==1 = [[a]]
                    |otherwise = createEmptyRoom (n-1) a ++ [[]] 

furnishRoom :: Int -> [Char] -> [[[Char]]]
furnishRoom n startingFurniture = furnishRoomHelper (createEmptyRoom n startingFurniture) 0 1 n

--create a method that concatenates
furnishRoomHelper :: [[[Char]]] -> Int -> Int -> Int -> [[[Char]]]
furnishRoomHelper currentRoom i j max 
                                      | (i==max || j == max)  = currentRoom
                                      | j == (max-1) && (i == 0) = furnishRoomHelper (conc currentRoom 0 i (getPossibleNeighbour (getFurnStat ((currentRoom !! i)!!(j-1))) [[],[]]) max) (i+1) 0 max
                                      | j == (max-1) = furnishRoomHelper(conc currentRoom 0 i (getPossibleNeighbour (getFurnStat ((currentRoom !! i)!!(j-1))) (getFurnStat ((currentRoom !! (i-1))!!j))) max) (i+1) 0 max  
                                      | (j == 0 )= furnishRoomHelper( conc currentRoom 0 i (getPossibleNeighbour [[],[]] (getFurnStat((currentRoom !! (i-1)) !! 0))) max) i (j+1) max
                                      | (i == 0) = furnishRoomHelper( conc currentRoom 0 i (getPossibleNeighbour (getFurnStat ((currentRoom !! 0)!!(j-1))) [[],[]]) max) i (j+1) max
                                      | otherwise = furnishRoomHelper(conc currentRoom 0 i (getPossibleNeighbour (getFurnStat ((currentRoom !! i)!!(j-1))) (getFurnStat ((currentRoom !! (i-1))!!j))) max) i (j+1) max
 
conc :: [[[Char]]] -> Int -> Int-> [Char] -> Int->[[[Char]]]
conc room idx target element max | (idx == max) = []
                                 | idx == target = [(room !! idx)++[element]] ++ conc room (idx+1) target element max
                                 | otherwise = [room !! idx] ++ conc room (idx+1) target element max
