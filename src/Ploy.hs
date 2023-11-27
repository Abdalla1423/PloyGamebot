module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift )
import Data.Char


-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################
--Das Ende des Spiels ist erreicht, wenn eine der beiden Parteien entweder
-- keinen Commander mehr besitzt, 
-- oder nur noch den Commander. 
--Die Partei, die den letzten Spielzugdurchgefuhrt hat, gewinnt.

gameFinished :: Board -> Bool
gameFinished b = if((searchCom b) == False) then
                   True
                else if((countFig b (Piece White 0)) == 1 ||(countFig b (Piece Black 0)) == 1) then
                  True
                else False
                

countFig :: Board -> Cell -> Int
countFig b c =
  let countFig' [] = 0
      countFig' (x:xs)
        | x == Empty = countFig' xs
        | (extractPlayer x) == (extractPlayer c) = 1 + countFig' xs
        | otherwise = countFig' xs
  in sum (map countFig' b)


extractPlayer :: Cell -> Player
extractPlayer (Piece p _) = p

searchCom :: Board -> Bool
searchCom b = searchForElement 0 0 False False
  where
    searchForElement r c foundb foundw
      | r == 9 = foundb && foundw
      | c == 9 = searchForElement (r+1) 0 foundb foundw
      | b !! r !! c == (Piece White 170) = searchForElement r (c+1) True foundw
      | b !! r !! c == (Piece White 85) = searchForElement r (c+1) True foundw
      | b !! r !! c == (Piece Black 170) = searchForElement r (c+1) foundb True
      | b !! r !! c == (Piece Black 85) = searchForElement r (c+1) foundb True
      | otherwise = searchForElement r (c+1) foundb foundw



-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################
--ist einer im Weg
-- welche Farbe ist im Ziel
-- keine Drehung größer 7
isValidMove :: Board -> Move -> Bool
isValidMove b m = if((turn m) <= 7) then -- gültiger Turn
                    --Wir bewegen uns
                    if((start m) /= (target m))then
                      --Keiner ist im Weg
                      if(noOneinWay (start m) (target m) b) then
                        --Das Ziel ist leer
                        if ((b!!(9 - (row (target m))))!!(colToIndex (col (target m))) == Empty) then
                          True 
                        --Das Ziel hat eine andere Farbe  
                        else if(extractPlayer ((b!!(9 - (row (start m))))!!(colToIndex (col (start m)))) /= extractPlayer ((b!!(9 - (row (target m))))!!(colToIndex (col (target m))))) then
                          True
                        --Das Ziel hat die selbe Farbe
                        else
                          False
                      --Jemand ist im Weg
                      else
                        False
                    --Wir bewegen uns nicht
                    else
                      True
                  -- ungültiger Turn
                  else 
                    False
                  

colToIndex :: Char -> Int
colToIndex c = ord c - ord 'a'

indexToCol :: Int -> Char
indexToCol i = chr(ord 'a' + i)

-- remove h and t and ten all should be empty
noOneinWay :: Pos -> Pos -> Board -> Bool
noOneinWay a b c = let l:ls = line a b in
                  if(isEmpty(init ls) c) then 
                    True
                  else
                    False

isEmpty :: [Pos] -> Board -> Bool
isEmpty [] _ = True
isEmpty (l:ls) b = if((b!!(9 - (row l)))!!(colToIndex (col l)) == Empty) then
                  isEmpty ls b
                else
                  False

-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################
--1 Bit auf 1: Shield
-- 2 Bit auf 1: Probe
-- 3 bit auf 1 : Lance
-- 4 Bit auf 1 bzw 170: Commander

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves p (Piece _ i) = if((countOnes i) == 1) then
                                possibleMovesShield p i
                              else if((countOnes i) == 2) then
                                possibleMovesProbe p i
                              else if((countOnes i) == 3) then
                                possibleMovesLance p i
                              else
                                possibleMovesCommander p i

possibleMovesShield :: Pos -> Int -> [Move]
possibleMovesShield p i = (allPossibleRotation p p i 1) ++ (filter inRange [allPossibleMovesSh p i 1]) ++ (filter inRange (allPossibleRotation p (target (allPossibleMovesSh p i 1)) i 1)) 


possibleMovesProbe :: Pos -> Int -> [Move]
possibleMovesProbe p i = (allPossibleRotation p p i 1) ++ (filter inRange (allPossibleMoves p i 1)) ++ (filter inRange (allPossibleMoves p i 2))


possibleMovesLance :: Pos -> Int -> [Move]
possibleMovesLance p i = (allPossibleRotation p p i 1) ++ (filter inRange (allPossibleMoves p i 1)) ++ (filter inRange (allPossibleMoves p i 2)) ++ (filter inRange (allPossibleMoves p i 3))

possibleMovesCommander :: Pos -> Int -> [Move]
possibleMovesCommander p i = (allPossibleRotation p p i 1) ++ (filter inRange (allPossibleMoves p i 1))

--possibleMovesShield p 1 = (filter inRange [(Move p (Pos (col p) ((row p) + 1)) 0)]) ++ (allPossibleRotation p p 1 1) ++ (filter inRange (allPossibleRotation p (Pos (col p) ((row p) + 1)) 1 1))
--possibleMovesShield p 2 = (filter inRange [(Move p (Pos (succ(col p)) ((row p) + 1)) 0)]) ++ (allPossibleRotation p p 2 1) ++ (filter inRange (allPossibleRotation p (Pos (succ(col p)) ((row p) + 1)) 2 1))
--possibleMovesShield p 4 = (filter inRange [(Move p (Pos (succ(col p)) (row p)) 0)]) ++ (allPossibleRotation p p 4 1) ++ (filter inRange (allPossibleRotation p (Pos (succ(col p)) (row p)) 4 1))
--possibleMovesShield p 8 =  (filter inRange [(Move p (Pos (succ(col p)) ((row p) - 1)) 0)]) ++ (allPossibleRotation p p 8 1) ++ (filter inRange (allPossibleRotation p (Pos (succ(col p)) ((row p) - 1)) 8 1))
--possibleMovesShield p 16 = (filter inRange [(Move p (Pos (col p) ((row p) - 1)) 0)]) ++ (allPossibleRotation p p 16 1) ++ (filter inRange (allPossibleRotation p (Pos (col p) ((row p) - 1)) 16 1))
--possibleMovesShield p 32 = (filter inRange [(Move p (Pos (pred(col p)) ((row p) - 1)) 0)]) ++ (allPossibleRotation p p 32 1) ++ (filter inRange (allPossibleRotation p (Pos (pred(col p)) ((row p) - 1)) 32 1))
--possibleMovesShield p 64 = (filter inRange [(Move p (Pos (pred(col p)) (row p)) 0)]) ++ (allPossibleRotation p p 64 1) ++ (filter inRange (allPossibleRotation p (Pos (pred(col p)) (row p)) 64 1))
--possibleMovesShield p 128 = (filter inRange [(Move p (Pos (pred(col p)) ((row p) + 1)) 0)]) ++ (allPossibleRotation p p 128 1) ++ (filter inRange (allPossibleRotation p (Pos (pred(col p)) ((row p) + 1)) 128 1))

--jetzige Posistion, Ausrichtung, Um wie viel moven
allPossibleMovesSh :: Pos -> Int -> Int -> Move
allPossibleMovesSh p i j = if((i .&. 1) == 1) then
                        (Move p (Pos (col p) ((row p) + j)) 0)
                      else if((i .&. 2) == 2) then
                        (Move p (Pos (chr(ord(col p)+j)) ((row p) + j)) 0)
                      else if((i .&. 4) == 4) then
                        (Move p (Pos (chr(ord(col p)+j)) (row p)) 0)
                      else if((i .&. 8) == 8) then
                        (Move p (Pos (chr(ord(col p)+j)) ((row p) - j)) 0)
                      else if((i .&. 16) == 16) then
                        (Move p (Pos (col p) ((row p) - j)) 0)
                      else if((i .&. 32) == 32) then
                        (Move p (Pos (chr(ord(col p)-j)) ((row p) - j)) 0)
                      else if((i .&. 64) == 64) then
                        (Move p (Pos (chr(ord(col p)-j)) (row p)) 0)
                      else
                        (Move p (Pos (chr(ord(col p)-j)) ((row p) + j)) 0)

allPossibleMoves :: Pos -> Int -> Int -> [Move]
allPossibleMoves p i j = if((i .&. 1) == 1) then
                        (Move p (Pos (col p) ((row p) + j)) 0) : allPossibleMoves p (i-1) j
                      else if((i .&. 2) == 2) then
                        (Move p (Pos (chr(ord(col p)+j)) ((row p) + j)) 0) : allPossibleMoves p (i-2) j
                      else if((i .&. 4) == 4) then
                        (Move p (Pos (chr(ord(col p)+j)) (row p)) 0) : allPossibleMoves p (i-4) j
                      else if((i .&. 8) == 8) then
                        (Move p (Pos (chr(ord(col p)+j)) ((row p) - j)) 0) : allPossibleMoves p (i-8) j
                      else if((i .&. 16) == 16) then
                        (Move p (Pos (col p) ((row p) - j)) 0) : allPossibleMoves p (i-16) j
                      else if((i .&. 32) == 32) then
                        (Move p (Pos (chr(ord(col p)-j)) ((row p) - j)) 0) : allPossibleMoves p (i-32) j
                      else if((i .&. 64) == 64) then
                        (Move p (Pos (chr(ord(col p)-j)) (row p)) 0) : allPossibleMoves p (i-64) j
                      else if((i .&. 128) == 128) then
                        (Move p (Pos (chr(ord(col p)-j)) ((row p) + j)) 0) : allPossibleMoves p (i-128) j
                      else
                        []

allPossibleRotation :: Pos -> Pos -> Int -> Int -> [Move]
allPossibleRotation oldpos newpos drehpos rot = if((rotate drehpos rot) ==  (rotate drehpos 0)) then
                                      []
                                    else
                                      (Move oldpos newpos rot) : allPossibleRotation oldpos newpos drehpos (rot + 1)

inRange :: Move -> Bool
inRange m = if((row(target m)) > 9 || (row(target m)) < 0 || (colToIndex (col (target m))) > 9 || (colToIndex (col (target m))) < 0) then
              False
            else
              True

countOnes :: Int -> Int
countOnes n = go n 0
  where
    go 0 acc = acc
    go n acc = go (n `div` 2) (acc + n `mod` 2)


-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves b p = if(gameFinished b) then
                  []
                else
                  searchMoves b 0 0 p

searchMoves :: Board -> Int -> Int -> Player -> [Move]
searchMoves b r c p =  if(r == 8) then
                        []
                      else if (c == 9) then
                        searchMoves b (r+1) 0 p                      
                      else if ((b!!r)!!c == Empty) then
                        searchMoves b r (c+1) p
                      else if((extractPlayer ((b!!r)!!c)) == p) then
                        (filter (isValidMove b) (possibleMoves (Pos (indexToCol c) (9-r)) ((b!!r)!!c))) ++ searchMoves b r (c+1) p
                      else
                        searchMoves b r (c+1) p
                     -- | r == 8 = []
                     -- | c == 9 = searchMoves b (r+1) 0 p
                     -- | (b!!r)!!c == Empty = searchMoves b r (c+1) p
                     -- | extractPlayer ((b!!r)!!c) == p = (filter (isValidMove b) (possibleMoves (Pos (indexToCol c) (9-r)) ((b!!r)!!c))) ++ searchMoves b r (c+1) p
                     -- | otherwise = searchMoves b r (c+1) p
                      
                