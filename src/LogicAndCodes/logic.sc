/* 46. Truth tables for logical expressions.
    Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false
    according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.

    scala> and(true, true)
    res0: Boolean = true

    scala> xor(true. true)
    res1: Boolean = false
*/
    def and1(x:Boolean, y:Boolean): Boolean = x && y
    def or1(x:Boolean, y:Boolean): Boolean = x || y
    def not1(x: Boolean): Boolean = !x
    def nand1(x:Boolean, y:Boolean): Boolean = not1(and1(x,y))
    def nor1(x:Boolean, y:Boolean): Boolean = not1(or1(x,y))
    def xor1(x:Boolean, y:Boolean): Boolean = (x && not1(y)) || (not1(x) && y)
    def impl1(x:Boolean, y:Boolean): Boolean = or1(not1(x), y)
    def equ1(x:Boolean, y:Boolean): Boolean = or1(and1(x,y),and1(not1(x),not1(y)))

/*
    A logical expression in two variables can then be written as an function of two variables,
    e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))

    Now, write a function called table2 which prints the truth table of a given logical expression in two variables.

    scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    A     B     result
    true  true  true
    true  false true
    false true  false
*/
  def table(f:(Boolean,Boolean)=>Boolean) ={
    println("A   B   Result")
    for {x <- List(true,false); y<- List(true,false)}{
      printf("%-5s %-5s %-5s\n", x, y, f(x, y))
    }
  }

table((a: Boolean, b: Boolean) => and1(a, or1(a, b)))

/*  47. Truth tables for logical expressions (2).
    Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit
    conversion from Boolean.) not will have to be left as a object method.

    scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
    A     B     result
    true  true  true
    true  false true
    false true  false
    false false false
*/


class Logic(a: Boolean) {
  import Logic._

  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _            => false
  }
  def or(b: Boolean): Boolean = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _         => false
  }

  def not(a:Boolean): Boolean = !a

  def equ(b: Boolean): Boolean = (a and b) or (not(a) and not(b))
  def xor(b: Boolean): Boolean = not(a equ b)
  def nor(b: Boolean): Boolean = not(a or b)
  def nand(b: Boolean): Boolean = not(a and b)
  def impl(b: Boolean): Boolean = not(a) or b
}

object Logic {
  implicit def booleanLogic(a: Boolean): Logic = new Logic(a)
}

/*  49. Gray code.
    An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
    n = 1: C(1) = ("0", "1").
    n = 2: C(2) = ("00", "01", "11", "10").
    n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").

    Find out the construction rules and write a function to generate Gray codes.

    scala> gray(3)
    res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)

    See if you can use memoization to make the function more efficient.
*/

var grays : Map[Int,List[String]] = Map()

def gray(i: Int) : List[String] = {
  grays.get(i) match {
    case Some(res) => res
    case _ =>
      val res = i match {
        case 0 => Nil
        case 1 => List("0", "1")
        case _ =>
          val others = gray(i - 1)
          val reflected = others.reverse
          others.map("0" + _) ++ reflected.map("1" + _)
      }
      grays = grays + (i -> res)
      res
  }
}

gray(2)

