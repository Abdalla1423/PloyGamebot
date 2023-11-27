-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################

import Test.Hspec

import Board
    ( buildBoard,
      line,
      validateFEN,
      Board,
      Cell(Empty, Piece),
      Player(Black, White),
      Pos(Pos) )
import Ploy ( gameFinished, isValidMove, listMoves, Move(Move), possibleMoves )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testGameFinished
    testIsValidMove
    testPossibleMoves
    testListMoves

sampleBoard :: Board
sampleBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

sampleBoard2 :: Board
sampleBoard2 = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]


testValidateFEN :: Spec
testValidateFEN = describe "Module Board: validateFEN ..." $ do
        it "fen has 2 rows" $ do
            validateFEN ",,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)
        it "fen has 11 rows" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)
        it "fen is right" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (True :: Bool)
        it "fen has a slash too much at the end" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/" `shouldBe` (False :: Bool)
        it "too few commas in first line" $ do
            validateFEN ",,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/" `shouldBe` (False :: Bool)
        it "example fen" $ do
            validateFEN ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69," `shouldBe` (True :: Bool)
        it "empty fen" $ do
            validateFEN "" `shouldBe` (False :: Bool)
testBuildBoard :: Spec
testBuildBoard = describe "Module Board: buildBoard ..." $ do
        it "build empty board" $ do
            buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
        it "Sample Board" $ do
            buildBoard ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69," `shouldBe` sampleBoard

testLine :: Spec
testLine = describe "Module Board: line ..." $ do
        it "start is target" $ do
            line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])
        it "vertical down" $ do
            line (Pos 'a' 5) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 5),(Pos 'a' 4), (Pos 'a' 3), (Pos 'a' 2),(Pos 'a' 1)] :: [Pos])
        it "vertical up" $ do
            line (Pos 'a' 1) (Pos 'a' 5) `shouldBe` ([(Pos 'a' 1),(Pos 'a' 2), (Pos 'a' 3), (Pos 'a' 4),(Pos 'a' 5)] :: [Pos])
        it "horizontal right" $ do
            line (Pos 'a' 5) (Pos 'f' 5) `shouldBe` ([(Pos 'a' 5),(Pos 'b' 5), (Pos 'c' 5), (Pos 'd' 5),(Pos 'e' 5),(Pos 'f' 5)] :: [Pos])
        it "horizontal left" $ do
            line (Pos 'f' 5) (Pos 'a' 5) `shouldBe` ([(Pos 'f' 5),(Pos 'e' 5), (Pos 'd' 5), (Pos 'c' 5),(Pos 'b' 5),(Pos 'a' 5)] :: [Pos])
        it "diagonal right down" $ do
            line (Pos 'c' 7) (Pos 'f' 4) `shouldBe` ([(Pos 'c' 7),(Pos 'd' 6), (Pos 'e' 5), (Pos 'f' 4)] :: [Pos])
        it "diagonal right up" $ do
            line (Pos 'f' 4) (Pos 'c' 7) `shouldBe` ([(Pos 'f' 4),(Pos 'e' 5), (Pos 'd' 6), (Pos 'c' 7)] :: [Pos])
        it "diagonal left down" $ do
            line (Pos 'i' 9) (Pos 'f' 6) `shouldBe` ([(Pos 'i' 9),(Pos 'h' 8), (Pos 'g' 7), (Pos 'f' 6)] :: [Pos])
        it "diagonal left up" $ do
            line (Pos 'f' 6) (Pos 'i' 9) `shouldBe` ([(Pos 'f' 6),(Pos 'g' 7), (Pos 'h' 8), (Pos 'i' 9)] :: [Pos])
    
testGameFinished :: Spec
testGameFinished = describe "Module Game: gameFinished ..." $ do
        it "start board not finished" $ do
            gameFinished sampleBoard `shouldBe` (False :: Bool)
        it "White only comrad 170" $ do
            gameFinished [[Empty,Empty,Empty,Empty,Piece White 170,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (True :: Bool)
        it "White only comrad 85" $ do
            gameFinished [[Empty,Empty,Empty,Empty,Piece White 85,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (True :: Bool)
        it "White no comrad" $ do
            gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (True :: Bool)
        it "Black only comrad 170" $ do
            gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Piece Black 170,Empty,Empty,Empty,Empty]] `shouldBe` (True :: Bool)
        it "Black only comrad 85" $ do
            gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Piece Black 85,Empty,Empty,Empty,Empty]] `shouldBe` (True :: Bool)
        it "Black no comrad" $ do
            gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Empty,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (True :: Bool)

testIsValidMove :: Spec
testIsValidMove = describe "Module Game: isValidMove ..." $ do
        it "rotation by 1 is always possible" $ do
            isValidMove sampleBoard (Move (Pos 'c' 1) (Pos 'c' 1) 1) `shouldBe` (True :: Bool)
        it "black on black" $ do
            isValidMove sampleBoard (Move (Pos 'b' 1) (Pos 'c' 1) 1) `shouldBe` (False :: Bool)
        it "white on white" $ do
            isValidMove sampleBoard (Move (Pos 'b' 9) (Pos 'c' 9) 1) `shouldBe` (False :: Bool)
        it "black on white" $ do
            isValidMove sampleBoard (Move (Pos 'e' 7) (Pos 'e' 3) 1) `shouldBe` (True :: Bool)
        it "mt on target" $ do
            isValidMove sampleBoard (Move (Pos 'e' 7) (Pos 'e' 6) 1) `shouldBe` (True :: Bool)
        it "someone is in my way!" $ do
            isValidMove sampleBoard (Move (Pos 'e' 7) (Pos 'e' 2) 1) `shouldBe` (False :: Bool)
        it "too much turn!" $ do
            isValidMove sampleBoard (Move (Pos 'e' 7) (Pos 'e' 6) 8) `shouldBe` (False :: Bool)

testPossibleMoves :: Spec
testPossibleMoves = describe "Module Game: possibleMoves ..." $ do
        it "Shield 1" $ do
            possibleMoves (Pos 'e' 5) (Piece White 1) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'e' 6) 0), (Move (Pos 'e' 5) (Pos 'e' 6) 1)] :: [Move])
        it "Shield 2" $ do
            possibleMoves (Pos 'e' 5) (Piece White 2) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 6) 0), (Move (Pos 'e' 5) (Pos 'f' 6) 1)] :: [Move])
        it "Shield 3" $ do
            possibleMoves (Pos 'e' 5) (Piece White 4) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 5) 0), (Move (Pos 'e' 5) (Pos 'f' 5) 1)] :: [Move])
        it "Shield 4" $ do
            possibleMoves (Pos 'e' 5) (Piece White 8) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 4) 0), (Move (Pos 'e' 5) (Pos 'f' 4) 1)] :: [Move])
        it "Shield 5" $ do
            possibleMoves (Pos 'e' 5) (Piece White 16) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'e' 4) 0), (Move (Pos 'e' 5) (Pos 'e' 4) 1)] :: [Move])
        it "Shield 6" $ do
            possibleMoves (Pos 'e' 5) (Piece White 32) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'd' 4) 0), (Move (Pos 'e' 5) (Pos 'd' 4) 1)] :: [Move])
        it "Shield 7" $ do
            possibleMoves (Pos 'e' 5) (Piece White 64) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'd' 5) 0), (Move (Pos 'e' 5) (Pos 'd' 5) 1)] :: [Move])
        it "Shield 128" $ do
            possibleMoves (Pos 'e' 5) (Piece White 128) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'd' 6) 0), (Move (Pos 'e' 5) (Pos 'd' 6) 1)] :: [Move])
        it "Probe 24" $ do
            possibleMoves (Pos 'e' 5) (Piece White 24) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 4) 0), (Move (Pos 'e' 5) (Pos 'e' 4) 0)] :: [Move])
        it "Probe 40" $ do
            possibleMoves (Pos 'e' 5) (Piece White 40) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 4) 0), (Move (Pos 'e' 5) (Pos 'd' 4) 0), (Move (Pos 'e' 5) (Pos 'g' 3) 0)] :: [Move])
        it "Probe 130" $ do
            possibleMoves (Pos 'e' 5) (Piece White 130) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 6) 0), (Move (Pos 'e' 5) (Pos 'd' 6) 0), (Move (Pos 'e' 5) (Pos 'g' 7) 0),(Move (Pos 'e' 5) (Pos 'c' 7) 0) ] :: [Move])
        it "Probe 17" $ do
            possibleMoves (Pos 'e' 5) (Piece White 17) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 3), (Move (Pos 'e' 5) (Pos 'e' 6) 0), (Move (Pos 'e' 5) (Pos 'e' 4) 0)] :: [Move])
        it "Lance 84" $ do
            possibleMoves (Pos 'e' 5) (Piece White 84) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 5) 0), (Move (Pos 'e' 5) (Pos 'e' 4) 0),(Move (Pos 'e' 5) (Pos 'd' 5) 0),(Move (Pos 'e' 5) (Pos 'g' 5) 0),(Move (Pos 'e' 5) (Pos 'e' 3) 0),(Move (Pos 'e' 5) (Pos 'c' 5) 0),(Move (Pos 'e' 5) (Pos 'h' 5) 0)] :: [Move])
        it "Lance 41" $ do
            possibleMoves (Pos 'e' 5) (Piece White 41) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'e' 6) 0), (Move (Pos 'e' 5) (Pos 'f' 4) 0)] :: [Move])
        it "Lance 56" $ do
            possibleMoves (Pos 'e' 5) (Piece White 56) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 7), (Move (Pos 'e' 5) (Pos 'f' 4) 0), (Move (Pos 'e' 5) (Pos 'e' 4) 0)] :: [Move])
        it "Lance 56 links oben" $ do
            possibleMoves (Pos 'a' 9) (Piece White 56) `shouldContain`([(Move (Pos 'a' 9) (Pos 'a' 9) 7), (Move (Pos 'a' 9) (Pos 'b' 8) 0), (Move (Pos 'a' 9) (Pos 'a' 8) 0)] :: [Move])
        it "Commander 170" $ do
            possibleMoves (Pos 'e' 5) (Piece White 170) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 1), (Move (Pos 'e' 5) (Pos 'f' 6) 0), (Move (Pos 'e' 5) (Pos 'f' 4) 0), (Move (Pos 'e' 5) (Pos 'd' 4) 0),(Move (Pos 'e' 5) (Pos 'd' 6) 0)] :: [Move])
        it "Commander 170 links oben" $ do
            possibleMoves (Pos 'a' 9) (Piece White 170) `shouldContain` ([(Move (Pos 'a' 9) (Pos 'a' 9) 1), (Move (Pos 'a' 9) (Pos 'b' 8) 0)] :: [Move])


testListMoves :: Spec
testListMoves = describe "Module Game: listMoves ..." $ do
        it "game finished" $ do
            listMoves sampleBoard2 Black `shouldBe` ([] :: [Move])
        it "startgame" $ do
            listMoves sampleBoard Black `shouldContain` ([(Move (Pos 'd' 3) (Pos 'd' 3) 1)] :: [Move])

        