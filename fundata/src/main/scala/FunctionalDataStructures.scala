object FunctionalDataStructures {
	case class Queue[A](front: List[A], back: List[A])
	
	def enqueue[A](elt: A, q: Queue[A]): Queue[A] = {
		Queue(q.front, (elt :: Nil) ::: q.back)
	}

	def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = q match {
		case Queue(Nil, Nil) => None
		case Queue(Nil, back) => Some((back.last), Queue(back.reverse.tail, Nil))
		case Queue(front, Nil) => Some((front.head), Queue(front.tail, Nil))
		case Queue(front, back) => Some((front.head), Queue(front.tail, back))
	}
	
	sealed trait JoinList[A] { val size: Int}
	
	case class Empty[A]() extends JoinList[A] { val size = 0}

	case class Singleton[A](elt: A) extends JoinList[A] { val size = 1}
	
	case class Join[A](lst1: JoinList[A], lst2: JoinList[A], size: Int) extends JoinList[A] 

	def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = lst match {
		case Empty() => None
		case Singleton(x) => Some(x)
		case Join(Empty(), Empty(), size) => None
		case Join(lst1, Empty(), size) => max(lst1, compare)
		case Join(Empty(), lst2, size) => max(lst2, compare)
		case Join(Singleton(x), Singleton(y), size) => {
			if(compare(x, y)) { Some(x) }
			else { Some((y)) }
		}
		case Join(a, b, size) => {
			(max(a, compare), max(b, compare)) match {
				case (Some(x), Some(y)) => {
					if(compare(x, y)) { Some(x) } 
					else { Some(y) }
				}
  				case (Some(x), None) => Some(x)
  				case (None, Some(y)) => Some(y)
  				case (None, None) => None
			}
		}
	}

	def first[A](lst: JoinList[A]): Option[A] = lst match {
		case Empty() => None
		case Singleton(x) => Some(x)
		case Join(Empty(), Empty(), size) => None
		case Join(lst1, Empty(), size) => first(lst1)
		case Join(Empty(), lst2, size) => first(lst2)
		case Join(a, b, size) => first(a)
	}

	def rest[A](lst: JoinList[A]): Option[JoinList[A]] = lst match {
		case Empty() => None
		case Join(Empty(), Empty(), size) => None
		case Singleton(x) => Some(Empty())
		case Join(Empty(), lst2, size) => {
			rest(lst2)
		}
		case Join(lst1, Empty(), size) => {
			rest(lst1)
		}
		case Join(Singleton(x), Singleton(y), size) => { Some(Singleton(y)) }
		case Join(a, b, c) => {
			Some(Join(restHelper(a), b, c-1))
		}
	}

	def restHelper[A](lst: JoinList[A]): JoinList[A] = lst match {
		case Empty() => Empty()
		case Singleton(x) => Empty()
		case Join(lst1, Empty(), size) => restHelper(lst1)
		case Join(Empty(), lst2, size) => restHelper(lst2)
		case Join(Singleton(x), Singleton(y), size) => Singleton(y)
		case Join(lst1, lst2, size) => {
			Join(restHelper(lst1), lst2, size)
		}
	}

	def nth[A](lst: JoinList[A], n: Int): Option[A] = lst match {
		case Empty() => None
		case Singleton(x) => {
			if(n == 0) { Some(x) }
			else { None }
		}
		case Join(Empty(), Empty(), size) => None
		case Join(lst1, lst2, size) => {
			if(n < 0) { None }
			if(n >= size) { None }
			else {
				if(n <= lst1.size - 1) {
					nth(lst1, n)
				}
				else {
					nth(lst2, n - lst1.size)
				}
			}
		}
	}

	def map[A,B](f: A => B, lst: JoinList[A]): JoinList[B] = lst match {
		case Empty() => Empty()
		case Singleton(x) => Singleton(f(x))
		case Join(Empty(), lst2, size) => Join(Empty(), map(f, lst2), size)
		case Join(lst1, Empty(), size) => Join(map(f, lst1), Empty(), size)
		case Join(lst1, lst2, size) => Join(map(f, lst1), map(f, lst2), size)
	}

	def filter[A](pred: A => Boolean, lst: JoinList[A]): JoinList[A] = lst match {
		case Empty() => Empty()
		case Singleton(x) => {
			if(pred(x)) { Singleton(x) }
			else { Empty() }
		} 
		case Join(Empty(), Empty(), size) => {
			Join(Empty(), Empty(), size)
		}
		case Join(Empty(), lst2, size) => {
			filter(pred, lst2)
		}
		case Join(lst1, Empty(), size) => {
			filter(pred, lst1)
		}
		case Join(lst1, lst2, size) => {
			Join(filter(pred, lst1), filter(pred, lst2), filter(pred, lst1).size+filter(pred, lst2).size)
		}
	}
}