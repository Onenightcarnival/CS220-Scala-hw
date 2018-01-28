import hw.generics._

	sealed trait BinTree[A] extends ListLike [A, BinTree[A]]
	case class Node[A](lhs: BinTree[A], value: A, rhs: BinTree[A]) extends BinTree[A]{

		def cons(head: A): BinTree[A] = {
			new Node(Leaf(), head, this)
		}
		
		def head(): Option[A] = lhs match {
			case Leaf() => Some(value)
			case _ => lhs.head()
		}
		
		def isEmpty(): Boolean = false

		def tail(): Option[BinTree[A]] = lhs match {
			case Leaf() => Some(rhs)
			case _ => Some(new Node(lhs.tail().get, value, rhs))
		}
	}
	case class Leaf[A]() extends BinTree [A] {

		def cons(head: A): BinTree[A] = new Node(Leaf(), head, Leaf())
		
		def head(): Option[A] = None
		
		def isEmpty(): Boolean = true

		def tail() : Option[BinTree[A]] = None
	}

	case class IntLike(a: Int) extends Ordered[IntLike]{
			def compare(other: IntLike): Ordering ={
				if(this.a > other.a) GT
				else if(this.a < other.a) LT
				else EQ
			}
	}

	object ListFunctions{
		def filter[A, C <: ListLike[A, C]](f: A => Boolean, alist: C): C = {
			if(alist.isEmpty()) alist
			else if(f(alist.head().get)) filter[A, C](f, alist.tail().get).cons(alist.head().get)
			else filter[A, C](f, alist.tail().get)
		} 

		def append[A, C <: ListLike[A, C]](alist1: C, alist2: C): C = {
			if(alist1.isEmpty()) alist2
			else append[A, C](alist1.tail().get, alist2).cons(alist1.head().get) 
		}

		def sort[A <: Ordered[A], C <: ListLike[A, C ]](alist: C): C = {
			if(alist.isEmpty()) alist
			else insertOrdered[A, C](alist.head().get, sort[A,C](alist.tail().get))
		}

		def insertOrdered[A <: Ordered[A], C <: ListLike[A, C]](toAdd: A, alist: C): C = {
			if(alist.isEmpty()) alist.cons(toAdd)
			else if(toAdd.compare(alist.head().get) == LT) alist.tail.get.cons(alist.head().get).cons(toAdd)
			else insertOrdered(toAdd, alist.tail().get).cons(alist.head().get)
		}
	}

	class C1 extends T2[Int, Int, String, String] with T3[Int,Int,Int,String,String,String,Int]{
		// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int , b: Int ): Int = 0
		def g(c: String ): String = ""
		def h(d: String ): Int = 0
	}

	class C2 extends T1[Int, Int] with T2[Int, Int, Int, Int] with T3[Int, Int, Int, Int, Int, Int, Int]{
		// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int , b: Int ): Int = 0
		def g(c: Int ): Int = 0
		def h(d: Int ): Int = 0
	}

	class C3[A](x: A) extends T3[Int, A, Int, A, String, String, A]{
		// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int , b: A): Int = 0
		def g(c: A): String = ""
		def h(d: String ): A = x
	}

	class C4[A](x: Int , y: C4[A]) extends T1[Int, C4[A]] with T3[Int,C4[A],C4[A],Int,C4[A],C4[A],Int]{
		// Do not change the class body . Simply extend T1 , T2 , and/or T3.
		def f(a: Int , b: C4[A ]): C4[A] = b
		def g(c: Int ): C4[A] = y
		def h(d: C4[A]): Int = x
	}