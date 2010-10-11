type Position = (Int,Int)


getPossiblePositions :: Position -> [Position]
getPossiblePositions position@(x,y) = filter valid 
	[
		(x+1,y+2),
		(x-1,y+2),
		(x+2,y+1),
		(x-2,y+1),

		(x+1,y-2),
		(x-1,y-2),
		(x+2,y-1),
		(x-2,y-1)
	]
	where
		valid (a,b) = a>=0 && b>=0 && a<boardSize && b<boardSize
					

boardSize=5
initBoard = take boardSize (repeat (take boardSize (repeat 0)))

mark board (x,y) = before ++ [(row (board!!y))] ++ after
	where
		before = take y board
		after  = drop (y+1) board
		row r = (take x r) ++ [1] ++ drop (x+1) r

isMarked board (x,y) = ((board!!y)!!x) == 1

visitedAll b = all (==True) (map (all (==1)) b)


solveTour :: Position -> (Bool,[Position])
solveTour (x,y) = solve initBoard [] (x,y)

solve board l (x,y) = if (allVisited || noMoreMoves) then (allVisited,newList)  else otherOptions
	where
		allVisited = visitedAll newBoard
		noMoreMoves = (length newPositions)==0
		newPositions = filter (not.(isMarked newBoard)) (getPossiblePositions (x,y))
		newBoard = mark board (x,y)
		newList = l ++ [(x,y)]

		otherOptions = oneOf (map (solve newBoard newList) (newPositions))

		oneOf [] = (False,[])
		oneOf (item@(t,list):xs) = if t then item else oneOf xs


display (b,xs) = show b ++ "\n" ++ (show xs)
