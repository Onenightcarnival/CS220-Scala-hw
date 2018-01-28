object Lists {
	
	val oddNumbers = 1 :: 3 :: 5 :: Nil 
	
	def sumDouble(aList: List[Int]): Int = aList match {
		case Nil => 0
		case head :: Nil => 2 * head 
		case head :: tail => 2 * head + sumDouble(tail)
		case _ => throw new Exception("Wrong Input")
	}	

	def removeZeroes(aList: List[Int]): List[Int] = aList match {
		case Nil => Nil
		case head :: Nil => {
			if (head == 0) {
				Nil
			}
			else {
				head :: Nil
			}
		}
		case head :: tail => {
			if (head == 0) {
				removeZeroes(tail)
			}
			else {
				head :: removeZeroes(tail)
			}
		}
		case _ => throw new Exception("Wrong Input")
	}

	def countEvens(aList: List[Int]): Int = aList match {
		case Nil => 0
		case head :: tail => {
			if (head % 2 ==0) {
				1 + countEvens(tail)
			}
			else {
				countEvens(tail)
			}
		}
		case _ => throw new Exception("Wrong Input")
	}

	def removeAlternation(aList: List[String]): List[String] = aList match {
		case Nil => Nil
		case a :: Nil => List(a)
		case a :: b :: Nil => List(a)
		case a :: b :: c :: Nil => List(a, c)
		case a :: b :: tail => a :: removeAlternation(tail)
		case _ => throw new Exception("Wrong Input")
	}

	def isAscending(aList: List[Int]): Boolean = aList match {
		case Nil => true
		case a :: Nil => true
		case a :: b :: Nil => {
			if (a <= b) {
				true
			}
			else {
				false
			}
		}
		case a :: tail => {
			val temp = tail.head
			if (a <= temp) {
				isAscending(tail)
			}
			else {
				false
			}
		}
		case _ => throw new Exception("Wrong Input")
	}

	def addSub(aList: List[Int]): Int = aList match {
		case Nil => 0
		case a :: Nil => a
		case a :: b :: Nil => a - b
		case a :: b :: tail => a - b + addSub(tail)
		case _ => throw new Exception("Wrong Input")
	}

	def alternate(aList: List[Int], bList: List[Int]): List[Int] = (aList, bList) match {
		case (Nil, Nil) => {
			Nil
		}
		case (a :: Nil, b :: Nil) => {
			a :: b :: Nil
		}
		case (a :: tail1, b :: tail2) => {
			a :: b :: (alternate(tail1, tail2))
		}
		case _ => {
			throw new Exception ("Wrong Input")
		}
	}

	def fromTo(x: Int, y: Int): List[Int] = {
			if (x >= y) {
				print("Wrong Input")
			}
			if (x == y-1) {
				List(x, y)
			}
			else {
				x :: fromTo(x+1, y)
			}
	}

	def insertOrdered(n: Int, lst: List[Int]): List[Int] = (n, lst) match{
		case (n: Int, Nil) => {
			n :: Nil
		}
		case (n: Int, head :: Nil) => {
			if (n == head) {
				head :: Nil
			}
			else if (n < head) {
				n :: head :: Nil
			}
			else {
				head :: n :: Nil
			}
		}
		case (n: Int, a :: tail) => {
			if (n <= a) {
				n :: a :: tail
			}
			else {
				a :: insertOrdered(n, tail)
			}
		}
	}

	def sort(lst: List[Int]): List[Int] = lst match{
		case Nil => Nil
		case a :: Nil => a :: Nil
		case a :: restpart => {
			if(a > restpart.head) {
				insertOrdered(a, sort(restpart))
			}
			else {
				insertOrdered(restpart.head, sort(a :: restpart.tail))
			}
		}
		case _ => throw new Exception("Wrong Input")
	}
	
}

