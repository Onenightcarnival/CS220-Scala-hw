import hw.sudoku._
	object Solution extends SudokuLike {
	
		type T = Board

		val emptyBoard = 0.to(8).map(x => 0.to(8).map(y => (x,y) -> List(1,2,3,4,5,6,7,8,9))).flatten.toMap

		def parse(str: String): Board = {
			new Board(parseHelper(str, 0, emptyBoard))
		}

		def parseHelper(str: String, x: Int, board: Map[(Int, Int), List[Int]]): Map[(Int, Int), List[Int]] = x match {
			case 80 => {
				if(str.charAt(x) == '.'){
					board
				}
				else {
					new Board(board).place(8, 8, str.charAt(80).asDigit).available
				}
			}
			case _ => {
					if(str.charAt(x) == '.'){
						parseHelper(str, x + 1, board)
					}
					else {
						parseHelper(str, x + 1, new Board(board).place(x/9, x%9, str.charAt(x).asDigit).available)
					}
			}
		}
		// You can use a Set instead of a List (or , any Iterable )
	
		def peers(row: Int, col: Int): List[(Int, Int)] = {
			rowHelper(row, 8).++(colHelper(8, col)).++(blockHelper(row, col)).distinct.filterNot(x => x == (row, col))
		}

		def rowHelper(row: Int, col: Int): List[(Int, Int)] = col match {
			case 0 => List((row, 0))
			case _ => rowHelper(row, col - 1).+:(row, col)
		}

		def colHelper(row: Int, col: Int): List[(Int, Int)] = row match {
			case 0 => List((0, col))
			case _ => colHelper(row - 1, col).+:(row, col)
		}

		def blockHelper(row: Int, col: Int): List[(Int, Int)] = {
			if(row/3 == 0 && col/3 == 0) {
				0.to(2).toList.map(x => 0.to(2).toList.map(y => (x, y))).flatten
			}
			else if(row/3 == 0 && col/3 == 1) {
				0.to(2).toList.map(x => 3.to(5).toList.map(y => (x, y))).flatten
			}
			else if(row/3 == 0 && col/3 == 2) {
				0.to(2).toList.map(x => 6.to(8).toList.map(y => (x, y))).flatten
			}
			else if(row/3 == 1 && col/3 == 0) {
				3.to(5).toList.map(x => 0.to(2).toList.map(y => (x, y))).flatten
			}
			else if(row/3 == 1 && col/3 == 1) {
				3.to(5).toList.map(x => 3.to(5).toList.map(y => (x, y))).flatten
			}
			else if(row/3 == 1 && col/3 == 2) {
				3.to(5).toList.map(x => 6.to(8).toList.map(y => (x, y))).flatten
			}
			else if(row/3 == 2 && col/3 == 0) {
				6.to(8).toList.map(x => 0.to(2).toList.map(y => (x, y))).flatten
			}
			else if(row/3 == 2 && col/3 == 1) {
				6.to(8).toList.map(x => 3.to(5).toList.map(y => (x, y))).flatten
			}
			else 6.to(8).toList.map(x => 6.to(8).toList.map(y => (x, y))).flatten
		}
	}
		// Top - left corner is (0 ,0). Bottom - right corner is (8 ,8). Feel free to
		// change the fields of this class .
	
	class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {
	
		def availableValuesAt(row : Int, col: Int): List[Int] = {
		// Assumes that a missing value means all values are available . Feel
		// free to change this .available . getOrElse (( row , col ), 1. to (9). toList )
			available.getOrElse((row,col),1.to(9).toList)
		}
	
		def valueAt(row: Int, col: Int): Option[Int] = {
			if(availableValuesAt(row, col).size != 1) {
				None
			}
			else {
				Some(availableValuesAt(row, col).head)
			}
		}
		
		def isSolved(): Boolean = {
			available.forall(x => x._2.size == 1)
		}
		
		def isUnsolvable(): Boolean = {
			available.exists(x => x._2.size == 0)
		}

		def place(row: Int, col: Int, value: Int): Board = {
			require(availableValuesAt(row, col).contains(value))
			new Board(placeHelper(value, Solution.peers(row, col), available+((row, col)->List(value))))
		}
		// You can return any Iterable (e.g., Stream )
		
		def placeHelper(value: Int, peers: List[(Int, Int)], available: Map[(Int, Int), List[Int]]): Map[(Int, Int), List[Int]] = peers match{
			case Nil => available
			case head :: tail => {
				if(available(head._1, head._2).contains(value)) {
					if((available+((head._1, head._2)->(available(head._1, head._2).filterNot(x => x == value))))(head._1, head._2).size == 1){
						placeHelper(value, tail, placeHelper((available+((head._1, head._2)->(available(head._1, head._2).filterNot(x => x == value))))(head._1, head._2).head, 
							Solution.peers(head._1, head._2), available+((head._1, head._2)->available(head._1, head._2).filterNot(x => x == value))))
					}
					else {
						placeHelper(value, tail, available+((head._1, head._2)->available(head._1, head._2).filterNot(x => x == value)))
					}
				}
				else {
					placeHelper(value, tail, available)
				}
			}
		}

		def nextStates(): Stream[Board] = {
  			if(isUnsolvable()) {
  				Stream[Board]()
  			}
  			else {
       			available.foldLeft(Stream[Board]())((x, y) => if(available(y._1).size == 1) x
          													  else x ++: available(y._1).foldLeft(Stream[Board]())((m, n) => m :+ this.place(y._1._1, y._1._2, n))).sortBy(x => x.available.foldLeft(0)((m, n) => m + x.available(n._1).size))
      		}
		}

		def solve(): Option[Board] = {
			if(isSolved()) Some(this)
			else solveHelper(this.nextStates)
		}

		def solveHelper(astream: Stream[Board]): Option[Board] = astream match{
  			case head #:: tail => {
    			if(head.solve != None) head.solve
    			else  solveHelper(tail)
  			}
  			case _ => None
		}
	}