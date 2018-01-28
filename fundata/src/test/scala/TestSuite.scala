class Tests extends org.scalatest.FunSuite { 

	import FunctionalDataStructures._
	
	val emptyQueue: Queue[Int] = Queue(List(), List())
	val oneQueue: Queue[Int] = Queue(List(1), List())
	val oneQueue2: Queue[Int] = Queue(List(), List(1))
	val twoQueue: Queue[Int] = Queue(List(1,2), List())
	val twoQueue2: Queue[Int] = Queue(List(1), List(2))
	val twoQueue3: Queue[Int] = Queue(List(), List(2,1))
	val threeQueue: Queue[Int] = Queue(List(1,2,3), List())
	val threeQueue2: Queue[Int] = Queue(List(1,2), List(3))
	val threeQueue3: Queue[Int] = Queue(List(1), List(3,2))
	val threeQueue4: Queue[Int] = Queue(List(), List(3,2,1))

	test("enqueue test") {
		assert(enqueue(4, emptyQueue) == Queue(List(), List(4)))
		assert(enqueue(4, oneQueue) == Queue(List(1), List(4)))
		assert(enqueue(4, oneQueue2) == Queue(List(), List(4,1)))
		assert(enqueue(4, twoQueue) == Queue(List(1,2), List(4)))
		assert(enqueue(4, twoQueue2) == Queue(List(1), List(4,2)))
		assert(enqueue(4, twoQueue3) == Queue(List(), List(4,2,1)))
		assert(enqueue(4, threeQueue) == Queue(List(1,2,3), List(4)))
		assert(enqueue(4, threeQueue2) == Queue(List(1,2), List(4,3)))
		assert(enqueue(4, threeQueue3) == Queue(List(1), List(4,3,2)))
		assert(enqueue(4, threeQueue4) == Queue(List(), List(4,3,2,1)))
	}

	test("dequeue test") {
		assert(dequeue(emptyQueue) == None)
		assert(dequeue(oneQueue) == Some(1, Queue(List(), List())))
		assert(dequeue(oneQueue2) == Some(1, Queue(List(), List())))
		assert(dequeue(twoQueue) == Some(1, Queue(List(2), List())))
		assert(dequeue(twoQueue2) == Some(1, Queue(List(), List(2))))
		assert(dequeue(twoQueue3) == Some(1, Queue(List(2), List())))
		assert(dequeue(threeQueue) == Some(1, Queue(List(2,3), List())))
		assert(dequeue(threeQueue2) == Some(1, Queue(List(2), List(3))))
		assert(dequeue(threeQueue3) == Some(1, Queue(List(), List(3,2))))
		assert(dequeue(threeQueue4) == Some(1, Queue(List(2,3), List())))
	}


	def fromList[A](alist: List[A]): JoinList[A] = alist match {
		case Nil => Empty()
		case List(x) => Singleton(x)
		case _ => {
			val len = alist.length
			val (lhs, rhs) = alist.splitAt(len/2)
			Join(fromList(lhs), fromList(rhs), len)
		}
	}

	def toList[A](alist: JoinList[A]): List[A] = alist match {
		case Empty() => Nil
		case Singleton(x) => List(x)
		case Join(alist1, alist2, _) => toList(alist1) ++ toList(alist2)
	}

	val emptyList: JoinList[Int] = Empty()
	val oneElt: JoinList[Int] = Singleton(1)
	val twoElt: JoinList[Int] = Join(Singleton(1), Singleton(2), 2)
	val threeElt: JoinList[Int] = Join(Join(Singleton(3), Singleton(4), 2), Singleton(7), 3)

	test("toList test") {
		assert(toList(threeElt) == List(3,4,7))
	}

	test("first test") {
		assert(first(emptyList) == None)
		assert(first(threeElt) == Some(3))
		assert(first(oneElt) == Some(1))
		assert(first(twoElt) == Some(1))
	}

	test("rest test") {
		assert(rest(emptyList) == None)
		assert(rest(threeElt) == Some(fromList(List(4,7))))
		assert(rest(oneElt) == Some(Empty()))
		
	}

	def gT(x: Int, y: Int): Boolean = x > y

	test("max test") {
		assert(max(emptyList, gT) == None)
		assert(max(oneElt, gT) == Some(1))
		assert(max(twoElt, gT) == Some(2))
		assert(max(threeElt, gT) == Some(7))
	}

	test("nth test") {
		assert(nth(emptyList, -1) == None)
		assert(nth(emptyList, 0) == None)
		assert(nth(emptyList, 1) == None)
		assert(nth(oneElt, -1) == None)
		assert(nth(oneElt, 1) == None)
		assert(nth(oneElt, 0) == Some(1))
		assert(nth(twoElt, -1) == None)
		assert(nth(twoElt, 2) == None)
		assert(nth(twoElt, 0) == Some(1))
		assert(nth(twoElt, 1) == Some(2))
		assert(nth(threeElt, -1) == None)
		assert(nth(threeElt, 3) == None)
		assert(nth(threeElt, 0) == Some(3))
		assert(nth(threeElt, 1) == Some(4))
		assert(nth(threeElt, 2) == Some(7))
	}

	def lT5(x: Int): Boolean = x < 5

	test("filter test") {
		assert(toList(filter(lT5, emptyList)) == Nil)
		assert(toList(filter(lT5, oneElt)) == List(1))
		assert(toList(filter(lT5, twoElt)) == List(1,2))
		assert(toList(filter(lT5, threeElt)) == List(3,4))
	}
}