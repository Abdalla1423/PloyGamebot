module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Chars
import Data.List.Split
import Data.List




-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN s = validateFENN s 0
--validateFEN s = let commaList = filter (== ',') s
 --               in if length commaList == 72 then 
 --                 let slashList = filter (== '/') s
 --                 in if length slashList == 8 then True else False 
  --              else False



validateFENN :: String -> Int -> Bool
validateFENN s 9 = False;
validateFENN [] _ = False;
validateFENN s c = let (commaSeparatedString, rest) = span (/= '/') s
                  in let commaList = filter (== ',') commaSeparatedString
                    in if length commaList == 8 then
                      if(c == 8 && rest == []) then
                         True
                      else validateFENN (tail rest) (c + 1)
                   else False





-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard s = let listOfLists = splitOn "/" s in 
                let twoDimensionalList = map (splitOn ",") listOfLists 
                --in liststoCell twoDimensionalList
                in map (map replaceElement) twoDimensionalList

replaceElement :: String -> Cell
replaceElement x = 
    -- Replace the element with "White" if it starts with "w"
    if 'w' `elem` x
        then (Piece White (read (tail x) :: Int))
    -- Replace the element with "Black" if it starts with "b"
    else if 'b' `elem` x
        then (Piece Black (read (tail x) :: Int))
    -- Replace the element with "Empty" if it doesn't start with "w" or "b"
    else Empty

-- Use the map function to split each substring into a list of individual elements


--liststoCell list = map (\innerList -> map (\str -> if (head str) == 'w' 
--  then (Piece White (read (tail str) :: Int)) 
--else if (head str) == 'b' 
--  then (Piece Black (read (tail str) :: Int)) 
--  else Empty) innerList) list
--liststoCell :: [[String]] -> Board
--liststoCell list = trace("HI") $ map (map liststoCell) list
--  where
--    liststoCell str
--      | "w" `isPrefixOf` str = trace(show str) $ (Piece White (read (tail str) :: Int))
--      | ((head str) == 'b') = (Piece Black (read (tail str) :: Int))
--      | otherwise = trace("Hallo") Empty
--buildBoard x:xs = if( x == ',' && xs == ',') then Empty  else if()



-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

line :: Pos -> Pos -> [Pos]
line a b = if(a == b) then [b]
          -- Entlag einer Zeile
          else if(((row a) /= (row b)) && ((col a) == (col b))) then
            changerow a b
          else if(((row a) == (row b)) && ((col a) /= (col b))) then
            changecol a b
          else do
            changernc a b


changerow :: Pos -> Pos -> [Pos]
changerow a b = if((row a) > (row b)) then
                a : line (Pos (col a) ((row a) - 1)) b
              else
                a : line (Pos (col a) ((row a) + 1)) b 

  
changecol :: Pos -> Pos -> [Pos]
changecol a b = if((col a) > (col b)) then
                a : line (Pos (pred (col a )) ((row a))) b
              else
                a : line (Pos (succ (col a)) ((row a))) b

changernc :: Pos -> Pos -> [Pos]
changernc a b = if((row a) > (row b) && (col a) > (col b)) then
                a : line (Pos (pred (col a )) ((row a) - 1)) b
              else if ((row a) < (row b) && (col a) > (col b)) then
                a : line (Pos (pred (col a )) ((row a) + 1)) b 
              else if ((row a) > (row b) && (col a) < (col b)) then
                a : line (Pos (succ (col a )) ((row a) - 1)) b
              else
                a : line (Pos (succ (col a )) ((row a) + 1)) b  

          

--line1 :: Pos -> Pos -> [Pos]
--line1 a b = if(a == b) then [b]
          -- Entlag einer Zeile
 --         else
--            if((row a) > (row b)) then
--                a : line1 (Pos (col a) ((row a) - 1)) b
--            else if((row b) > (row a)) then
--                a : line1 (Pos (col a) ((row a) + 1)) b
          -- Entlang einer Spalte  
--            if((col a) > (col b)) then
--                a : line1 (Pos (pred (col a )) ((row a))) b
--            else if((col b) > (col a)) then
--                a : line1 (Pos (succ (col a)) ((row a))) b