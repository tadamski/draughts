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
             putStrLn "=========================================="
             printMoveHelp
             putStrLn "=========================================="
             moveStr <- getLine
             if isWalkStr moveStr
                then do
                   let whiteMove = parseWalk moveStr
                   if validateWhiteWalk board whiteMove
                      then continueRound board whiteMove
                      else do
                         putStrLn "Invalid move!"
                         gameRound board       
                else if isJumpStr moveStr
                   then do
                      let whiteMove = parseJump moveStr
                      if validateWhiteJump board whiteMove
                         then continueRound board whiteMove
                         else do
                            putStrLn "Invalid move!"
                            gameRound board
                   else putStrLn "Invalid move!"
                               

continueRound :: Board -> Move -> IO()
continueRound board whiteMove =
   do
      let boardAfterWhite = performMove board whiteMove
      let blackMoves = possibleBlackMoves boardAfterWhite boardAfterWhite
      i <- randomMove (length blackMoves)
      let boardAfterBlack = performMove boardAfterWhite (blackMoves!!(i-1))
      printBoard boardAfterBlack
      gameRound boardAfterBlack
             --putStrLn "Invalid move!"
             --gameRound board

isWalkStr :: String -> Bool
isWalkStr moveStr = (elemIndex '-' moveStr)/=Nothing

isJumpStr :: String -> Bool
isJumpStr moveStr = (elemIndex 'x' moveStr)/=Nothing
          
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

printMoveHelp :: IO()
printMoveHelp = 
              do
                 putStrLn ".1.2.3.4"
                 putStrLn "5.6.7.8"
                 putStrLn ".9.0.1.2"
                 putStrLn "3.4.5.6"
                 putStrLn ".7.8.9.0"
                 putStrLn "1.2.3.4"
                 putStrLn ".5.6.7.8"
                 putStrLn "9.0.1.2"


printStone :: Stone -> IO()                                 
printStone stone = case stone of
                        White -> putChar 'w'
                        Black -> putChar 'b'
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

performMove :: Board -> Move -> Board
performMove board move = if isWalk move then performWalk board move else performJump board move

isWalk :: Move -> Bool
isWalk ((sr,sc),(tr,tc)) = (abs (tr-tc))==1

performWalk :: Board -> Move -> Board
performWalk board (source,target) = replaceOnBoard board [(source,Empty),(target,(findStone board source))]

performJump :: Board -> Move -> Board
performJump board ((sr,sc),(tr,tc)) = replaceOnBoard board [((sr,sc),Empty),((div (sr+tr) 2, div (sc+tc) 2),Empty),((tr,tc),(findStone board (sr,sc)))]

replaceOnBoard :: Board -> [(Position,Stone)] -> Board
replaceOnBoard [] replacements = []
replaceOnBoard board replacements = (checkReplaceField (head board) replacements) : (replaceOnBoard (tail board) replacements)

checkReplaceField :: (Position,Stone) -> [(Position,Stone)] -> (Position,Stone)
checkReplaceField field [] = field
checkReplaceField field (replacement:rest) = if fst(field)==fst(replacement) then replacement else checkReplaceField field rest

parseWalk :: String -> Move
parseWalk moveStr = (fieldNoToPosition(read(take d moveStr)), fieldNoToPosition(read(drop (d+1) moveStr))) where d = (fromJust (elemIndex '-' moveStr))

validateWhiteWalk :: Board -> Move -> Bool
validateWhiteWalk board (source,target) = 
                  case (findStone board source) of 
                       White -> validateWhitePawnWalk board (source,target)
                       _ -> False

validateWhitePawnWalk :: Board -> Move -> Bool
validateWhitePawnWalk board ((sr,sc),(tr,tc)) =
                  (tr-sr==1) && ((abs (tc-sc))==1) && ((findStone board (tr,tc))==Empty) 


validateWhiteJump :: Board -> Move -> Bool
validateWhiteJump board (source,target) =
                  case (findStone board source) of 
                       White -> validateWhiteJump board (source,target)
                       _ -> False

validateWhitePawnJump :: Board -> Move -> Bool
validateWhitePawnJump board ((sr,sc),(tr,tc)) =
                  (tr-sr==2) && ((abs (tc-sc))==2) && ((findStone board (div (tr-sr) 2,div (tc-sc) 2))==Black) &&((findStone board (tr,tc))==Empty)



parseJump :: String -> Move
parseJump moveStr = (fieldNoToPosition(read(take d moveStr)), fieldNoToPosition(read(drop (d+1) moveStr))) where d = (fromJust (elemIndex 'x' moveStr))

possibleBlackMoves :: Board -> Board -> [Move]
possibleBlackMoves board [] = []
possibleBlackMoves board ((position, stone) : rest) = if stone==Black
                                             then (checkBlackMoves board position) ++ (possibleBlackMoves board rest)
                                             else [] ++ (possibleBlackMoves board rest)
                                                      
randomMove :: Int -> IO Int
randomMove range = randomRIO (1,range) 
