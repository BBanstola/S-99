// 01. Find the last element of a list.

val list1 = List(1,2,3,4,5)
list1.last

// 02. Find the last but one element of a list. Second last

val list2 = List(1,2,3,4,5,6,5,4,2,3)

def lastu(l: List[Int]) = l.init.last

lastu(list2)

def lastt[T](l: List[T]):T = l match {
  case Nil => throw new Error ("Empty List")
  case h ::_:: Nil => h
  case _ :: t => lastt(t)
  case _ => throw new NoSuchElementException
}

lastt(list2)

// 03. Find the Kth element of a list.

def find[T](l:List[T], index: Int):T= l(index)

find(list2, 5)

// 04. Find the number of elements of a list.

def len[T](l:List[T]):Int = l.length

len(list1)

// 05. Reverse a list.

def rev[T](l:List[T]):List[T] = l.reverse

rev(list1)

//  06. Find out whether a list is a palindrome.
val list3 = List(1,2,3,2,1)

def isPalin[T](l:List[T]):Boolean = if (l==l.reverse) true else false

isPalin(list3)

// 07. Flatten a nested list structure.

val l1 = List(1,2,3,3,'a','b',List(1,2,3),4)

def flat(ls: List[Any]): List[Any] = ls flatMap {
  case ms: List[_] => flat(ms)
  case q => List(q)
}

flat(l1)

l1.flatMap(_.toString)             //Similar result but not recommended

// 08. Eliminate consecutive duplicates of list elements.

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

// 09. Pack consecutive duplicates of list elements into sublists.

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

// 10. Use the result of earlier problem to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

def encode[A](ls: List[A]): List[(Int, A)] =
  pack(ls) map { a => (a.length, a.head) }

encode(l2)

// 11. Modify the result of earlier problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

def modifiedEncode[A](l: List[A]):List[Any] =
  encode(l) map {a => if (a._1 == 1)a._2 else (a._1,a._2)}

modifiedEncode(l2)

// 12. Given a run-length code list generated as specified in encode , construct its uncompressed version.

def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap
  { e => List.fill(e._1)(e._2) }

decode(encode(l2))

// 13. Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
def encodeDirect[A](l: List[A]):List[(Int, A)] = {
  if (l.isEmpty) Nil
  else {
    val(same, next) = l span ( _ == l.head)
    (same.length, same.head)::encodeDirect(next)
  }
}

encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

// 14 & 15. Duplicate the elements of a list.

def duplicate[A](count:Int,l: List[A]):List[A] = l flatMap {
  {e => List.fill(count)(e)}
}
duplicate(3,List('a','b','c','d'))

//  16. Drop every Nth element from a list.

def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
  def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
    case (_, Nil)       => Nil
    case (1, _ :: tail) => dropR(n, tail)
    case (_, h :: tail) => h :: dropR(c - 1, tail)
  }
  dropR(n, ls)
}

dropRecursive(3,List('a','b','c','d'))


// 17. Split a list into two parts.

def split[T](pos:Int, l:List[T]) = (l.take(pos),l.drop(pos))

split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))


// 18. Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.

def slice[A](n1: Int, n2:Int, l: List[A]): List[A] = (l.drop(n1)).take(n2-n1)

slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

// 19. Rotate a list N places to the left.

def rotate[A](n: Int, l: List[A]): List[A] = {
  if (n < 0) {
    val m = l.length + n
    ((l.drop(m):::l.take(m)))
  }
  else{
    ((l.drop(n):::l.take(n)))}}

rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

rotate(-3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

//  20. Remove the Kth element from a list. from 0.
def removeAt[A](pos: Int, l:List[A]) = (l.take(pos):::l.drop(pos+1),l(pos))

removeAt(1, List('a, 'b, 'c, 'd, 'e, 'f, 'g))

// 21. Insert an element at a given position into a list.
def insertAt[A](insertion: A, pos: Int, l: List[A]):List[A] = l.splitAt(pos) match {
  case (pre, post) => pre ::: insertion ::post
}

insertAt('new, 1, List('a, 'b, 'c, 'd))

// 22.  Create a list containing all integers within a given range

def ranging(n1:Int, n2:Int):List[Int] =
  if (n1 > n2)
    List.range(n2,n1+1)
  else
    List.range(n1,n2+1)

ranging(5,1)

// 23. Extract a given number of randomly selected elements from a list.

def randomSelect1[A](n: Int, ls: List[A]): List[A] =
  if (n <= 0) Nil
  else {
    val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
    e :: randomSelect1(n - 1, rest)
  }

randomSelect1(3,list2)

//  24. Lotto: Draw N different random numbers from the set 1..M.

def lotto(num: Int, limit:Int):List[Int] = {
  val s = List.range(1,limit)
  randomSelect1(num, s)
}

lotto(5,50)

// 25. Generate a random permutation of the elements of a list.

def randomPermute[A](l:List[A]):List[A] = {
  randomSelect1(l.length,l)
}

randomPermute(list3)

/* 26. Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people?
We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial
coefficient). For pure mathematicians, this result may be great. But we want to really generate
all the possibilities. */

def combo[A](num:Int, l: List[A]) ={
 l.combinations(num).mkString("\n")
}

combo(2,List(1,2,3,4))

/*  27. Group the elements of a set into disjoint subsets.
    a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
    Write a function that generates all the possibilities.
*/

def comboAll[A](l:List[A])={
  var count = l.length
  while (count > 0){
    println(combo(count,l))
    count -= 1
  }
}

comboAll(List(1,2,3,4,5))

// Combination another way

def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
  ls match {
    case Nil => Nil
    case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
  }

def combinations[A](n: Int, ls: List[A]): List[List[A]] =
  if (n == 0) List(Nil)
  else flatMapSublists(ls) { sl =>
    combinations(n - 1, sl.tail) map {sl.head :: _}
  }


/* Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will
    return a list of groups. Example:
    scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...*/

def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
  case Nil     => List(Nil)
  case n :: ns => combinations(n, ls) flatMap { c =>
    group(ns, ls diff c) map {c :: _}
  }
}

group(List(2,3),List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))

/* 28. Sorting a list of lists according to length of sublists.
    We suppose that a list contains elements that are lists themselves.
    The objective is to sort the elements of the list according to their length.
    E.g. short lists first, longer lists later, or vice versa. */

def sortList[A](l:Array[Array[A]])= {             //Tried using conventional way (bubble sort)
  for (i <- 0 until l.length) {
    for (j <- 0 until i) {
      if (l(j).length < l(j + 1).length) {
        var temp = l(j)
        l(j) = l(j + 1)
        l(j + 1) = temp
      }
    }
  }
}


sortList(Array(Array('a, 'b, 'c), Array('d, 'e), Array('f, 'g, 'h), Array('d, 'e), Array('i, 'j, 'k, 'l), Array('m, 'n), Array('o)))

// Functional approach

def lSort[A](ls: List[List[A]]): List[List[A]] =
  ls sortWith { _.length < _.length }

def lSort1[A](ls: List[List[A]]): List[List[A]] = ls sortBy(_.length)

lSort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))

lSort1(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o, 'p)))

/* Again, we suppose that a list contains elements that are lists themselves.
   But this time the objective is to sort the elements according to their length frequency;
   i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others
  with a more frequent length come later. */

def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
  val freqs = Map(encode(ls map { _.length } sortWith  { _ < _ }) map { _.swap }:_*)
  ls sortWith  { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
}

lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))