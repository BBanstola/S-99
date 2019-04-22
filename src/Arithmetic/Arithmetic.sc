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

import scala.collection.mutable.ListBuffer

def primeFactors(n: Int): List[Int] = {
  var ans = new ListBuffer[Int]()
  for (i <- 2 to n/2){
    if (n % i == 0 && isPrime(i)) ans += i
  }
  val ansList = ans.toList
  ansList
}

primeFactors(315)

// 36. Determine the prime factors of a given positive integer (2).

def primeM(n: Int)={
  var ans = new ListBuffer[(Int,Int)]()
  for (i <- 0 until primeFactors(n).length){
    for (j<- 1 to n/2){
          if (n % (primeFactors(i)*j) == 0)
    }
  }
}




