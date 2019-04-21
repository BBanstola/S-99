//Find the last element of a list.

val list1 = List(1,2,3,4,5)
list1.last

// Find the last but one element of a list. Second last

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

// Find the Kth element of a list.

def find[T](l:List[T], index: Int):T= l(index)

find(list2, 5)

//  Find the number of elements of a list.

def len[T](l:List[T]):Int = l.length

len(list1)

//Reverse a list.

def rev[T](l:List[T]):List[T] = l.reverse

rev(list1)

//  Find out whether a list is a palindrome.
val list3 = List(1,2,3,2,1)

def isPalin[T](l:List[T]):Boolean = if (l==l.reverse) true else false

isPalin(list3)

// Split a list into two parts.

def split[T](pos:Int, l:List[T]) = (l.take(pos),l.drop(pos))

split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

//  Remove the Kth element from a list. from 0.
def removeAt[A](pos: Int, l:List[A]) = (l.take(pos):::l.drop(pos+1),l(pos))

removeAt(1, List('a, 'b, 'c, 'd, 'e, 'f, 'g))

// Insert an element at a given position into a list.
def insertAt[A](insertion: A, pos: Int, l: List[A]):List[A] = l.splitAt(pos) match {
  case (pre, post) => pre ::: insertion ::post
}

insertAt('new, 1, List('a, 'b, 'c, 'd))

// Create a list containing all integers within a given range

def ranging(n1:Int, n2:Int):List[Int] =
  if (n1 > n2)
    List.range(n2,n1+1)
  else
    List.range(n1,n2+1)

ranging(5,1)

//Extract a given number of randomly selected elements from a list.

def randomSelect1[A](n: Int, ls: List[A]): List[A] =
  if (n <= 0) Nil
  else {
    val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
    e :: randomSelect1(n - 1, rest)
  }

randomSelect1(3,list2)

//  Lotto: Draw N different random numbers from the set 1..M.

def lotto(num: Int, limit:Int):List[Int] = {
  val s = List.range(1,limit)
  randomSelect1(num, s)
}

lotto(5,50)

// Generate a random permutation of the elements of a list.

def randomPermute[A](l:List[A]):List[A] = {
  randomSelect1(l.length,l)
}

randomPermute(list3)

/* Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people?
We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial
coefficient). For pure mathematicians, this result may be great. But we want to really generate
all the possibilities. */

def combo[A](num:Int, l: List[A]) ={
 l.combinations(num).mkString("\n")
}

combo(2,List(1,2,3,4))

/*  Group the elements of a set into disjoint subsets.
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


