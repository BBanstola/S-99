// Flatten a nested list structure.

val l1 = List(1,2,3,3,'a','b',List(1,2,3),4)

def flat(ls: List[Any]): List[Any] = ls flatMap {
  case ms: List[_] => flat(ms)
  case q => List(q)
}

flat(l1)

l1.flatMap(_.toString)             //Similar result but not recommended


// Eliminate consecutive duplicates of list elements.

def compressTailRecursive[A](ls: List[A]): List[A] = {
  def compressR(result: List[A], curList: List[A]): List[A] = curList match {
    case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
    case Nil       => result.reverse
  }
  compressR(Nil, ls)
}