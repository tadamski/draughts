initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w." 

main = parseBoardStr initialBoardStr []

data Stone = White | Black | Empty deriving (Eq,Ord,Enum,Show)

type Position = (Int, Int) 

type Board = [(Position, Stone)]

parseBoardStr [] board = board
parseBoardStr boardStr board = parseBoardLines (lines boardStr) 1 board
                                    
parseBoardLines [] r board = board
parseBoardLines boardLines r board = (parseBoardLine (r 0) (head boardLines) board) ++ (parseBoardLines (tail boardLines) (r+1) board)
                                    
parseBoardLine (r, c) [] board = board
parseBoardLine (r, c) boardLine board =  case head boardLine of
                                        "w" -> ((r c) White) ++ (parseBoardLine (r c+1) (tail boardLine) board)
                                        "b" -> ((r c) Black) ++ (parseBoardLine (r c+1) (tail boardLine) board)
                                        "." -> ((r c) Empty) ++ (parseBoardLine (r c+1) (tail boardLine) board) 

printBoard [] = return ()
printBoard (((r, c), stone) : rest) = do 
                                        printStone stone
                                        if c==8 then 
                                                        putChar '\n'
                                                        printBoard rest
                                                   else
                                                        printBoard rest
                                 
printStone stone = case stone of
                        White -> putChar 'W'
                        Black -> putChar 'B'
                        Empty -> putChar '.'
                        
                        
                                            
                                        






