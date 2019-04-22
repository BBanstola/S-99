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



