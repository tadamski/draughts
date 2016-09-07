import Data.List
import Data.Maybe
import System.Random

initialBoardStr = ".w.w.w.w\nw.w.w.w.\n.w.w.w.w\n........\n........\nb.b.b.b.\n.b.b.b.b\nb.b.b.b." 

-- main = parseBoardStr initialBoardStr []

add :: Int -> Int -> Int
add a b = a + b

main = do 
          let board = parseBoardStr initialBoardStr []
          printBoard board
          gameRound board


data Stone = White | Black | Empty deriving (Eq,Ord,Enum,Show)

type Position = (Int, Int) 

type Board = [(Position, Stone)]

type Move = (Position, Position)

gameRound :: Board -> IO()
gameRound board =
          do
             moveStr <- getLine
             let boardAfterWhite = whiteMove board moveStr
             let blackMoves = possibleBlackMoves boardAfterWhite boardAfterWhite
             i <- randomMove (length blackMoves)
             let boardAfterBlack = performWalk boardAfterWhite (blackMoves!!(i-1))
             printBoard boardAfterBlack
             gameRound boardAfterBlack  
          

whiteMove :: Board -> String -> Board
whiteMove board moveStr =
          performWalk board (parseMove moveStr)


{-
blackMove :: Board -> Board
blackMove board =
          do 
             let blackMoves = possibleBlackMoves board board
             let l = length blackMoves
             i <- randomMove l 
             performWalk board (blackMoves!!i)
-}          

parseBoardStr :: String -> Board -> Board
parseBoardStr boardStr board = parseBoardRows (lines boardStr) 1 board
                                    

parseBoardRows :: [String] -> Int ->  Board -> Board
parseBoardRows [] r board = board
parseBoardRows boardRows r board = (parseBoardRow (r, 1) (head boardRows) board) ++ (parseBoardRows (tail boardRows) (r+1) board)

parseBoardRow :: Position -> String -> Board -> Board                                    
parseBoardRow (r, c) [] board = board
parseBoardRow (r, c) boardRow board =  case head boardRow of
                                        'w' -> [((r, c), White)] ++ (parseBoardRow (r, c+1) (tail boardRow) board)
                                        'b' -> [((r, c), Black)] ++ (parseBoardRow (r, c+1) (tail boardRow) board)
                                        '.' -> [((r, c), Empty)] ++ (parseBoardRow (r, c+1) (tail boardRow) board) 



printBoard :: Board -> IO()
printBoard [] = return ()
printBoard (((r, c), stone) : rest) = do 
                                        printStone stone
                                        if c==8 

                                            then do 
                                                putChar '\n'
                                                printBoard rest
                                            else

                                                printBoard rest


printStone :: Stone -> IO()                                 
printStone stone = case stone of
                        White -> putChar 'W'
                        Black -> putChar 'B'
                        Empty -> putChar '.'


findStone :: Board -> Position -> Stone
findStone board position = if fst(p)==position
                           then snd(p)
                           else findStone (tail board) position
			   where p=head board
         
                  

possibleMoves :: Board -> Position -> [Move]
possibleMoves board source = case stone of
                                White -> checkWhiteMoves board source
                                Black -> checkBlackMoves board source
                               where stone = findStone board source

onBoard :: Position -> Bool
onBoard (r,c) = (r>=1) && (r<=8) && (c>=1) && (c<=8)


checkWalk :: Board -> Move -> Bool
checkWalk board move = if (onBoard target) && ((findStone board target) == Empty) then True else False where target = snd(move)

checkJump :: Board -> Stone -> Move -> Bool
checkJump board beaten ((sr,sc), (tr,tc))  = (onBoard (tr,tc)) && ((findStone board (tr,tc)) == Empty) && (findStone board (div (sr+tr) 2, div (sc+tc) 2) == beaten)

checkWhiteMoves :: Board -> Position -> [Move]
checkWhiteMoves board (r,c) = (filter (checkWalk board) [(source,(r+1,c-1)),(source,(r+1,c+1))]) ++ (filter (checkJump board Black) [(source,(r+2,c-2)),(source,(r+2,c+2))]) where source=(r,c)

checkBlackMoves :: Board -> Position -> [Move]
checkBlackMoves board (r,c) = (filter (checkWalk board) [(source,(r-1,c-1)),(source,(r-1,c+1))]) ++ (filter (checkJump board White) [(source,(r-2,c-2)),(source,(r-2,c+2))]) where source=(r,c)

fieldNoToPosition :: Int -> Position 
fieldNoToPosition f = (r, if (mod r 2) == 0 then 2*(mod (f-1) 4)+1 else 2*(mod (f-1) 4)+2) where r = (div (f-1) 4)+1

performWalk :: Board -> Move -> Board
performWalk board move = replaceOnBoard board [(fst(move),Empty),(snd(move),(findStone board (fst move)))]

replaceOnBoard :: Board -> [(Position,Stone)] -> Board
replaceOnBoard [] replacements = []
replaceOnBoard board replacements = (checkReplaceField (head board) replacements) : (replaceOnBoard (tail board) replacements)

checkReplaceField :: (Position,Stone) -> [(Position,Stone)] -> (Position,Stone)
checkReplaceField field [] = field
checkReplaceField field (replacement:rest) = if fst(field)==fst(replacement) then replacement else checkReplaceField field rest

parseMove :: String -> Move
parseMove moveStr = (fieldNoToPosition(read(take d moveStr)), fieldNoToPosition(read(drop (d+1) moveStr))) where d = (fromJust (elemIndex '-' moveStr))

possibleBlackMoves :: Board -> Board -> [Move]
possibleBlackMoves board [] = []
possibleBlackMoves board ((position, stone) : rest) = if stone==Black
                                             then (checkBlackMoves board position) ++ (possibleBlackMoves board rest)
                                             else [] ++ (possibleBlackMoves board rest)
                                                      
randomMove :: Int -> IO Int
randomMove range = randomRIO (1,range) 
