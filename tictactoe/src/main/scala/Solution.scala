import hw.tictactoe._

case class Game(/* add fields here */turn: Player, dim: Int, board: Map[(Int, Int), Player]) extends GameLike[Game] { 
	
	def getBoard: Map[(Int, Int), Player] = board

	def getTurn: Player = turn

	def getDim: Int = dim

	def isFinished (): Boolean = {
		if(getWinner == None){
			if(board.size == dim * dim) true
			else false
		}
		else true
	}
	/* Assume that isFinished is true */

	def getWinner(): Option[Player] = {
		if(getRowWinner(X, dim - 1, board)){
			Some(X)
		}
		else if(getRowWinner(O, dim -1, board)){
			Some(O)
		}
		else if(getColWinner(X, dim - 1, board)){
			Some(X)
		}
		else if(getColWinner(O, dim - 1, board)){
			Some(O)
		}
		else if(getDiagonalWinner(X, dim - 1, board)){
			Some(X)
		}
		else if(getDiagonalWinner(O, dim - 1, board)){
			Some(O)
		}
		else if(getAntiDiagonalWinner(X, dim - 1, board)){
			Some(X)
		}
		else if(getAntiDiagonalWinner(O, dim - 1, board)){
			Some(O)
		}
		else None
	}

	def getRowWinner(name: Player, row: Int, board: Map[(Int, Int), Player]): Boolean =  row match {
		case 0 => (getRowWinnerHelper(name, 0, board).size == dim)
		case _ => {
			if(getRowWinnerHelper(name, row, board).size == dim) true
			else getRowWinner(name, row - 1, board)
		}
	}

	def getRowWinnerHelper(name: Player, row: Int, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
		board.filterKeys(x => x._2 == row).filter({ case (m, n) => (n == name)})
	}

	def getColWinner(name: Player, col: Int, board: Map[(Int, Int), Player]): Boolean = col match {
		case 0 => (getColWinnerHelper(name, 0, board).size == dim)
		case _ => {
			if(getColWinnerHelper(name, col, board).size == dim) true
			else getColWinner(name, col - 1, board)
		} 
	}

	def getColWinnerHelper(name: Player, col: Int, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
		board.filterKeys(x => x._1 == col).filter({ case (m, n) => (n == name)})
	}

  	def getDiagonalWinner(name: Player, row: Int, board: Map[(Int, Int), Player]): Boolean = {
  		if(getDiagonalWinnerHelper(name, row, board).size == dim) true
  		else false
  	}

  	def getDiagonalWinnerHelper(name: Player, row: Int, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
   		board.filterKeys(x => x._2 == x._1).filter({ case (m, n) => (n == name) })
  	}

  	def getAntiDiagonalWinner(name: Player, row: Int, board: Map[(Int, Int), Player]): Boolean = {
  		if(getAntiDiagonalWinnerHelper(name, row, board).size == dim) true
  		else false
  	}
	
	def getAntiDiagonalWinnerHelper(name: Player, row: Int, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
    	board.filterKeys(x => x._2 + x._1 == dim - 1).filter({ case (m, n) => (n == name) })
 	}

	def nextBoards(): List[Game] = {
		if(isFinished()){
			Nil
		}
		else{
			nextBoardsHelper(dim - 1, dim - 1, board)
		}
	}

	def nextBoardsHelper(col: Int, row: Int, board: Map[(Int, Int), Player]): List[Game] = (col, row) match {
		case (0, 0) => {
			if(!board.contains(0, 0)){
				if (getTurn == X) new Game(O, getDim, getBoard.+((0, 0) -> X)) :: Nil
				else new Game(X, getDim, getBoard.+((0, 0) -> O)) :: Nil
			}
			else Nil
		}
		case (x, 0) => {
			if(!board.contains(col, 0)){
				if(getTurn == X) new Game(O, getDim, getBoard.+((col, 0) -> X)) :: nextBoardsHelper(col - 1, dim - 1, board)
				else new Game(X, getDim, getBoard.+((col, 0) -> O)) :: nextBoardsHelper(col - 1, dim - 1, board)
			}
			else nextBoardsHelper(col - 1, dim - 1, board)
		}
		case (x, y) => {
			if(!board.contains(col, row)){
				if(getTurn == X) new Game(O, getDim, getBoard.+((col, row) -> X)) :: nextBoardsHelper(col, row - 1, board)
				else new Game(X, getDim, getBoard.+((col, row) -> O)) :: nextBoardsHelper(col, row - 1, board)
			}
			else nextBoardsHelper(col, row - 1, board)
		}
	}
}

object Solution extends MinimaxLike {
	type T = Game // T is an "abstract type member" of MinimaxLike
	
	def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game =  {
		new Game(turn, dim, board)
	}

	def minimax(board: Game): Option[Player] = {
      	if(board.isFinished()) board.getWinner()
      	else{
      		if(board.getTurn == X){
      			val temp = board.nextBoards()
      			val tempMap = temp.map(x => minimax(x))
      			if(tempMap.contains(Some(X))) Some(X)
      			else if(tempMap.contains(None)) None
      			else Some(O)
 			}
 			else {
 				val temp = board.nextBoards()
      			val tempMap = temp.map(x => minimax(x))
      			if(tempMap.contains(Some(O))) Some(O)
      			else if(tempMap.contains(None)) None
      			else Some(X)
 			}
 		}
    }
	/*
	If it is Xs turn:
	1. If X has won the game, return Some(X).
	2. If the game is a draw, return None. (If all squares are filled
	and nobody has won, then the game is a draw. However, you are
	free to detect a draw earlier , if you wish .)
	3. Recursively apply minimax to all the successor states of game
	- If any recursive call produces X, return Some(X)
	- Or , if any recursive call produces None , return None - Or, return Some(O)
	The case for Os turn is similar. 
	*/
}