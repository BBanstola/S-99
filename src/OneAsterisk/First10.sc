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


