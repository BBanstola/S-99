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

def decode[A](ls: List[(Int, A)]): List[A] =
  ls flatMap { e => List.fill(e._1)(e._2) }

decode(encode(l2))







