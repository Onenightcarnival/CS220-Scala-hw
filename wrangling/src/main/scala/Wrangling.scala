import List._
import Set._
import edu.umass.cs.CSV 

object Wrangling {
	val allBirths = CSV.fromFile("shortened-births.csv")
	val allExpect = CSV.fromFile("cdc-life-expectancy.csv")


	def yearIs(data: List[List[String]], n: Int): List[List[String]] = {
		data.filter(x => x(0).toInt == n)
	}

	def yearGT(data: List[List[String]], bound: Int): List[List[String]] = {
		data.filter(x => x(0).toInt >= bound)
	}

	def yearLT(data: List[List[String]], bound: Int): List[List[String]] = {
		data.filter(x => x(0).toInt <= bound)
	}

	def onlyName(data: List[List[String]], name: String): List[List[String]] = {
		data.filter(x => x(1) == name)
	}

	def mostPopular(data: List[List[String]]): (String, Int) = {
		(sortList(gt, transform(data.groupBy(x => x(1)).toList))).last
	}

	def transform(data: List[(String, List[List[String]])]): List[(String, Int)] = data match {
		case Nil => Nil
		case a :: tail => {
			(a._1, count(a._2)) :: transform(tail)
		}
	}

	def gt(x: Int, y: Int): Boolean = x > y

	def insert(lessThan: (Int, Int) => Boolean, x: (String, Int), alist: List[(String, Int)]): List[(String, Int)] = alist match { 
		case Nil => List(x)
		case hd :: tl => {
			if (lessThan(hd._2, x._2)) { x :: hd :: tl }
			else { hd :: insert(lessThan , x, tl) } 
		}
	}

	def sortList(lessThan: (Int, Int) => Boolean, alist: List[(String, Int)]): List[(String, Int)] = alist match { 
		case Nil => Nil
		case a :: Nil => a :: Nil
		case a :: restpart => {
			if(lessThan(a._2, (restpart.head)._2)) {
				insert(lessThan, restpart.head, sortList(lessThan, a :: restpart.tail))
			}
			else {
				insert(lessThan, a, sortList(lessThan, restpart))
			}
		}
		case _ => Nil
	}

	def count(data: List[List[String]]): Int = data match {
		case Nil => 0
		case a :: Nil => a(3).toInt
		case a :: tail => a(3).toInt + count(tail)
	}

	def countGirlsAndBoys(data: List[List[String]]): (Int, Int) = {
		(count(data.filter(x => x(2) == "F")), count(data.filter(x => x(2) == "M")))
	} 

	def genderNeutralNames(data: List[List[String]]): Set[String] = {
		val a = getName(splitGender(data, "F")).toSet
		val b = getName(splitGender(data, "M")).toSet
		a.intersect(b)
	}

	def splitGender(data: List[List[String]], gender: String): List[List[String]] = {
		data.filter(x => x(2) == gender)
	}

	def getName(data: List[List[String]]): List[String] = data match {
		case Nil => Nil
		case a :: Nil => a(1) :: Nil
		case a :: tail => a(1) :: getName(tail)
	}

	def expectedAlive(gender: String, birthYear: Int, currentYear: Int): Boolean = {
		if(gender == "F") {
			if((yearIs(allExpect, birthYear)(0))(2).toInt + birthYear >= currentYear) { true}
			else {false}
		}
		else {
			if((yearIs(allExpect, birthYear)(0))(1).toInt + birthYear >= currentYear) { true}
			else { false}
		}
	}
	
	def estimatePopulation(data: List[List[String]], year: Int): Int = data match {
		case Nil => 0
		case a :: tail => {
			if(expectedAlive(a(2), a(0).toInt, year)) {
				a(3).toInt + estimatePopulation(tail, year)
			}
			else {
				estimatePopulation(tail, year)
			}
		}
	}

}