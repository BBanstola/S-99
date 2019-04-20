// Flatten a nested list structure.

val l1 = List(1,2,3,3,'a','b',List(1,2,3),4)

def flat(ls: List[Any]): List[Any] = ls flatMap {
  case ms: List[_] => flat(ms)
  case q => List(q)
}

flat(l1)

l1.flatMap(_.toString)             //Similar result but not recommended


// Eliminate consecutive duplicates of list elements.

//Left- recursive way
def compressTailRecursive[A](ls: List[A]): List[A] = {
  def compressR(result: List[A], curList: List[A]): List[A] = curList match {
    case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
    case Nil       => result.reverse
  }
  compressR(Nil, ls)
}

compressTailRecursive(l1)

//simple recursion

def comp[A](ls: List[A]): List[A] = ls match {
  case Nil => Nil
  case h::t => h::comp(t.dropWhile(_ == h))
}

comp(l1)

//Pack consecutive duplicates of list elements into sublists.

val l2 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

def pack[A](l: List[A]):List[List[A]] = {
  if (l.isEmpty) List(List())
  else
    {
      val (packed, next) = l span( _ == l.head)
      if (next == Nil) List(packed)
      else packed::pack(next)
    }
}

pack(l2)

//Use the result of earlier problem to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

def encode[A](ls: List[A]): List[(Int, A)] =
  pack(ls) map { a => (a.length, a.head) }

encode(l2)

// Modify the result of earlier problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

def modifiedEncode[A](l: List[A]):List[Any] =
  encode(l) map {a => if (a._1 == 1)a._2 else (a._1,a._2)}

modifiedEncode(l2)

// Given a run-length code list generated as specified in encode , construct its uncompressed version.

def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap
  { e => List.fill(e._1)(e._2) }

decode(encode(l2))

// Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
def encodeDirect[A](l: List[A]):List[(Int, A)] = {
  if (l.isEmpty) Nil
  else {
    val(same, next) = l span ( _ == l.head)
    (same.length, same.head)::encodeDirect(next)
  }
}

encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

// Duplicate the elements of a list.

def duplicate[A](count:Int,l: List[A]):List[A] = l flatMap {
  {e => List.fill(count)(e)}
}
duplicate(3,List('a','b','c','d'))

//  Drop every Nth element from a list.

def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
  def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
    case (_, Nil)       => Nil
    case (1, _ :: tail) => dropR(n, tail)
    case (_, h :: tail) => h :: dropR(c - 1, tail)
  }
  dropR(n, ls)
}

dropRecursive(3,List('a','b','c','d'))

//Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.

def slice[A](n1: Int, n2:Int, l: List[A]): List[A] = (l.drop(n1)).take(n2-n1)

slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

//Rotate a list N places to the left.

def rotate[A](n: Int, l: List[A]): List[A] = {
  if (n < 0) {
    val m = l.length + n
    ((l.drop(m):::l.take(m)))
  }
  else{
  ((l.drop(n):::l.take(n)))}}

rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

rotate(-3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

