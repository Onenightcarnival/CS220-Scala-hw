object HOF {
	
	def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = (lst1, lst2) match {
		case (Nil, Nil) => Nil
		case (head1 :: tail1, head2 :: tail2) => {
			f(head1, head2) :: map2[A,B,C](f: (A, B) => C, tail1, tail2)
		}
		case _ => Nil
	} 

	def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1, lst2) match {
		case (Nil, Nil) => Nil
		case (a :: Nil, b :: Nil) => {
			List[(A,B)]((a, b))
		}
		case (a :: tail1, b :: tail2) => {
			(a, b) :: zip[A,B](tail1, tail2)
		}
		case _ => Nil
	}

	def flatten[A](alist: List[List[A]]): List[A] = alist match {
		case Nil => Nil
		case head :: Nil => head
		case head :: tail => {
			append[A](head, flatten[A](tail))
		} 
		case _ => Nil
	}

	def flatten3[A](alist: List[List[List[A]]]): List[A] = alist match {
		case Nil => Nil
		case head :: Nil => {
			flatten[A](head)
		}
		case head :: tail  =>{
			append[A](flatten[A](head), flatten3[A](tail))
		}
		case _ => Nil
	}

	def append[A](alist: List[A], blist: List[A]): List[A] = alist match {
		case Nil => blist
		case head :: tail => head :: append[A](tail, blist)
		case _ => Nil
	}

	def buildList[A](length: Int, f: Int => A): List[A] = {
		if (length <= 0) Nil
		else append(buildList[A](length - 1, f), List(f(length-1)))
	}

	def mapList[A, B](alist: List[A], f: A => List[B]): List[B] = alist match {
		case Nil => Nil
		case head :: tail => {
			append[B](f(head), mapList(tail, f))
		}
	}

	def partition[A](f: A => Boolean, alist: List[A]): (List[A], List[A]) = alist match {
		case Nil => (Nil, Nil)
		case head :: tail => {
			(helperTrue(f, alist), helperFalse(f, alist))
		}
		case _ => (Nil, Nil)
	}

	def helperTrue[A](f: A => Boolean, alist: List[A]): List[A] = alist match {
		case Nil => Nil
		case head :: tail => {
			if(f(head)) {
				head :: helperTrue[A](f, tail)
			}
			else {
				helperTrue[A](f, tail)
			}
		}
		case _ => Nil
	}

	def helperFalse[A](f: A => Boolean, alist: List[A]): List[A] = alist match {
		case Nil => Nil
		case head :: tail => {
			if(f(head)) {
				helperFalse[A](f, tail)
			}
			else {
				head :: helperFalse[A](f, tail)
			}
		}
		case _ => Nil
	}
	
	def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = (alist1, alist2) match {
		case (Nil, Nil) => Nil
		case (head :: Nil, Nil) => head :: Nil
		case (Nil, head :: Nil) => head :: Nil
		case (head :: tail, Nil) => {
			if(lessThan(head, tail.head)) {
				sort[A](lessThan, merge(lessThan, head :: Nil, tail))
			}
			else {
				sort[A](lessThan, merge(lessThan, tail.head :: Nil, head :: tail.tail))
			}
		}
		case (Nil, head :: tail) => {
			if(lessThan(head, tail.head)) {
				sort[A](lessThan, merge(lessThan, head :: Nil, tail))
			}
			else {
				sort[A](lessThan, merge(lessThan, tail.head :: Nil, head :: tail.tail))
			}
		}
		case (head1 :: Nil, head2 :: Nil) => {
			if (lessThan(head1, head2)) { head2 :: head1 :: Nil}
			else { head1 :: head2 :: Nil}
		}
		case (head1 :: Nil, head2 :: tail) => {
			if(lessThan(head1, head2)) {
				sort[A](lessThan, merge[A](lessThan, tail, head2 :: head1 :: Nil))
			}
			else {
				sort[A](lessThan, merge[A](lessThan, tail, head1 :: head2 :: Nil))
			}
		}
		case (head1 :: tail, head2 :: Nil) => {
			if(lessThan(head1, head2)) {
				sort[A](lessThan, head2 :: head1 :: merge[A](lessThan, tail, List[A]()))
			}
			else {
				sort[A](lessThan, head1 :: head2 :: merge[A](lessThan, tail, List[A]()))
			}
		}
		case (head1 :: tail1, head2 :: tail2) => {
			if (lessThan(head1, head2)) { sort(lessThan, head2 :: head1 :: merge(lessThan, tail1, tail2)) }
			else { sort(lessThan, head1 :: head2 :: merge(lessThan, tail1, tail2))}
		}
		
	}
	
	def insert[A](lessThan: (A, A) => Boolean, x: A, alist: List[A]): List[A] = alist match { 
		case Nil => List(x)
		case hd :: tl => {
			if (lessThan(hd, x)) { x :: hd :: tl }
			else { hd :: insert[A](lessThan , x, tl) } 
		}
	}

	def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = alist match { 
		case Nil => Nil
		case a :: Nil => a :: Nil
		case a :: restpart => {
			if(lessThan(a, restpart.head)) {
				insert[A](lessThan, restpart.head, sort[A](lessThan, a :: restpart.tail))
			}
			else {
				insert[A](lessThan, a, sort[A](lessThan, restpart))
			}
		}
		case _ => Nil
	}
}


