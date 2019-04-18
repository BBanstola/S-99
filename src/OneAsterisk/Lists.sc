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



