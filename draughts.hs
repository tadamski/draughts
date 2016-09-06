initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w." 

-- main = parseBoardStr initialBoardStr []

add :: Int -> Int -> Int
add a b = a + b

main = do 
          let board = parseBoardStr initialBoardStr []
          let stone = findStone board (1,2)
          printStone stone

data Stone = White | Black | Empty deriving (Eq,Ord,Enum,Show)

type Position = (Int, Int) 

type Board = [(Position, Stone)]

type Move = (Position, Position)

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
checkWalk board move = if (onBoard target) && ((findStone target) == Empty) then True else False where target = snd(move)

checkJump :: Board -> Move -> Stone -> Bool
checkJump board ((sr,sc), (tr,tc)) beaten  = (onBoard (tr,tc)) && ((findStone (tr,tc)) == Empty) && (findStone (((sr+tr)/2),((sc+tc)/2)) == beaten)

checkWhiteMoves :: Board -> Position -> [Move]
checkWhiteMoves board (r,c) = (filter (checkWalk) [(r+1,c-1),(r+1),(c+1)]) ++ (filter (checkJump) [(r+2,c-2),(r+2,c+2)])

checkBlackMoves :: Board -> Position -> [Move]
checkBlackMoves board (r,c) = (filter (checkWalk) [(r-1,c-1),(r-1),(c+1)]) ++ (filter (checkJump) [(r-2,c-2),(r-2,c+2)])









                                                                                                       
