// 31. Determine whether a given integer number is prime.

def isPrime(n:Int):Boolean = (2 until n) forall(x => n % x != 0)

isPrime(67)

// 32. Determine the greatest common divisor of two positive integer numbers. Use euclid problem.

def gcd(n1: Int, n2: Int):Int = if (n1 == 0) n2 else if (n2 == 0) n1 else gcd(n2, n1 % n2)

gcd(15,30)

// 33. Determine whether two positive integer numbers are coprime.

def isCoPrime(n1: Int, n2: Int):Boolean = if (gcd(n1,n2) == 1) true else false

isCoPrime(2,33)

/* 34. Calculate Euler's totient function phi(m).
   Euler's so-called totient function phi(m) is defined as the number of
   positive integers r (1 <= r <= m) that are coprime to m. */

def phi(n: Int): Int = {
  var count = 0
  for (i <- 1 to n){
    if (isCoPrime(n,i)) count += 1
  }
  count
}

phi(10)

// Alternate approach

def phi2(n: Int): Int = (1 to n) count { isCoPrime(_, n) }

phi2(10)

//  35. Determine the prime factors of a given positive integer.

import scala.collection.mutable.ListBuffer            //Traditional way, not applicable for multiple prime factors

def primeFactors(n: Int): List[Int] = {
  var ans = new ListBuffer[Int]()
  for (i <- 2 to n/2){
    if (n % i == 0 && isPrime(i)) ans += i
  }
  val ansList = ans.toList
  ansList
}

primeFactors(315)

def primeFactor(number: Int, list: List[Int] = List()): List[Int] = {
    for(n <- 2 to number if (number % n == 0)) {
      return primeFactor(number / n, list :+ n)
    }
    list
}

primeFactor(315)

// 36. Determine the prime factors of a given positive integer (2).

def encodeDirect[A](l: List[A]):List[(Int, A)] = {      // For simplification purpose
  if (l.isEmpty) Nil
  else {
    val(same, next) = l span ( _ == l.head)
    (same.length, same.head)::encodeDirect(next)
  }
}

def primeM(n: Int)={
  (encodeDirect(primeFactor(n))) map (_.swap)
}

primeM(315)

/* 37. Calculate Euler's totient function phi(m) (improved).
    See problem P34 for the definition of Euler's totient function. If the list of the prime factors
    of a number m is known in the form of problem P36 then the function phi(m>) can be efficiently calculated
    as follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their multiplicities)
    of a given number m. Then phi(m) can be calculated with the following formula:

    phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...

    Note that ab stands for the bth power of a.
*/

def eTotient(num : Int):Int ={
  primeM(num).foldLeft(1){ (r,f) => f match {case (p,m) => r* (p-1)* Math.pow(p, m-1).toInt} }
}

eTotient(10)

/* Compare the two methods of calculating Euler's totient function.
    Use the solutions of problems P34 and P37 to compare the algorithms.
     Try to calculate phi(10090) as an example.
*/

val primes = Stream.cons(2, Stream.from(3, 2) filter {isPrime(_)})

/*
def time[A](label: String)(block: => A): A = {
  val now = System.currentTimeMillis()
  val ret = block
  println(label + ": " + (System.currentTimeMillis() - now) + " ms.")
  ret
}

def test(n: Int) {
  time("Preload primes") {
    primes takeWhile { _ <= Math.sqrt(n) } force
  }
  time("P34 (" + n +  ")") {
    phi(n)
  }

  time("P34.2 (" + n + ")") {
    phi2(n)
  }
  time("P37 (" + n + ")") {
    eTotient(n)
  }
}

test(10090)

*/

// 39. Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

def primeRange(r: Range): List[Int] = primes dropWhile {_ < r.start } takeWhile {_ <= r.last} toList

primeRange(5 to 100)

/* 40.  Goldbach's conjecture.
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
 E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be
 correct in the general case. It has been numerically confirmed up to very large numbers (much larger
 than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given
 even integer.

*/

def goldBach(n:Int):(Int,Int) = primes takeWhile {_ < n}  find {p => isPrime(n - p)} match {
  case None => throw new Error ("Not applicable for this case")
  case Some(p1) => (p1, n - p1 )
}

goldBach(888888)

/* 41.  A list of Goldbach compositions.
    Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

    scala> printGoldbachList(9 to 20)
    10 = 3 + 7
    12 = 5 + 7
    14 = 3 + 11
    16 = 3 + 13
    18 = 5 + 13
    20 = 3 + 17

    In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely,
    the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.

    Example (minimum value of 50 for the primes):

    scala> printGoldbachListLimited(1 to 2000, 50)
    992 = 73 + 919
    1382 = 61 + 1321
    1856 = 67 + 1789
    1928 = 61 + 1867
*/

def goldBachRange(r: Range)={
  for (i <- r.start until r.last)
    if (i % 2 == 0) println( i +"="+ goldBach(i)._1 +"+"+ goldBach(i)._2 + "\n")
}

goldBachRange(22 to 48)

// faster way

def printGoldbachListLimited(r: Range, limit: Int) {
  (r filter { n => n > 2 && n % 2 == 0 } map { n => (n, goldBach(n)) }
    filter { _._2._1 >= limit } foreach {
    _ match { case (n, (p1, p2)) => println(n + " = " + p1 + " + " + p2) }
  })
}

printGoldbachListLimited(8 to 22,2)