package regexplay

import scala.math.min

object Semiring {

  trait Semiring[S] {
    def zero: S
    def one: S
    def add(a: S, b: S): S
    def mult(a: S, b: S): S
  }

  def sum[S: Semiring](l: List[S]): S = {
    val sr = summon[Semiring[S]]
    l.foldLeft(sr.zero)(sr.add(_, _))
  }

  def prod[S: Semiring](l: List[S]): S = {
    val sr = summon[Semiring[S]]
    l.foldLeft(sr.one)(sr.mult(_, _))
  }

  given semiringBool: Semiring[Boolean] with {
    def zero = false
    def one = true
    def add(a: Boolean, b: Boolean): Boolean = a || b
    def mult(a: Boolean, b: Boolean): Boolean = a && b
  }

  given semiringInt: Semiring[Int] with {
    def zero = 0
    def one = 1
    def add(a: Int, b: Int): Int = a + b
    def mult(a: Int, b: Int): Int = a * b
  }

  trait SemiringI[S] extends Semiring[S] {
    def index(i: Int): S
  }

//
//  /////////////////////// Leftmost /////////////////////
//
  sealed trait LeftmostT
  case object NoLeft extends LeftmostT
  case class Leftmost(s: StartT) extends LeftmostT

  sealed trait StartT
  case object NoStart extends StartT
  case class Start(pos: Int) extends StartT

  given semiringLeftmost: Semiring[LeftmostT] with {
    def zero = NoLeft
    def one = Leftmost(NoStart)

    def leftmost(x: StartT, y: StartT): StartT = {
      (x, y) match {
        case (NoStart, NoStart) => NoStart
        case (NoStart, Start(i)) => Start(i)
        case (Start(i), NoStart) => Start(i)
        case (Start(i), Start(j)) => Start(min(i, j))
      }
    }

    def add(a: LeftmostT, b: LeftmostT): LeftmostT = {
      (a, b) match {
        case (NoLeft, x) => x
        case (x, NoLeft) => x
        case (Leftmost(x), Leftmost(y)) => Leftmost(leftmost(x, y))
      }
    }

    def start(x: StartT, y: StartT): StartT = {
      (x, y) match {
        case (NoStart, s) => s
        case (s, _) => s
      }
    }

    def mult(a: LeftmostT, b: LeftmostT): LeftmostT = {
      (a, b) match {
        case (NoLeft, _) => NoLeft
        case (_, NoLeft) => NoLeft
        case (Leftmost(x), Leftmost(y)) =>  Leftmost(start(x, y))
      }
    }
  }

  given semiringILeftmost: SemiringI[LeftmostT] with {
    export semiringLeftmost.*
    override def index(i: Int): LeftmostT = Leftmost(Start(i))
  }

  //
  //  /////////////////////// LeftLong /////////////////////
  //
  sealed trait LeftLongT
  case object NoLeftLong extends LeftLongT
  case class LeftLong(r: RangeT) extends LeftLongT
  
  sealed trait RangeT
  case object NoRange extends RangeT
  case class Range(start: Int, end: Int) extends RangeT
  
  given semiringLeftLong: Semiring[LeftLongT] with {
    def zero = NoLeftLong
    def one = LeftLong(NoRange)
    
    def leftlong(x: RangeT, y: RangeT): RangeT = {
      (x, y) match {
        case (NoRange, NoRange) => NoRange
        case (NoRange, Range(i, j)) => Range(i, j)
        case (Range(i, j), NoRange) => Range(i, j)
        case (Range(i, j), Range(k, l)) => if (i < k || i == k && j >= l) Range(i, j) else Range(k, l)
      }
    }
    
    def add(a: LeftLongT, b: LeftLongT): LeftLongT = {
      (a, b) match {
        case (NoLeftLong, x) => x
        case (x, NoLeftLong) => x
        case (LeftLong(x), LeftLong(y)) => LeftLong(leftlong(x, y))
      }
    }
    
    def range(x: RangeT, y: RangeT): RangeT = {
      (x, y) match {
        case (NoRange, NoRange) => NoRange
        case (NoRange, r) => r
        case (r, NoRange) => r
        case (Range(i, _), Range(_, j)) => Range(i, j) 
      }
    }
    
    def mult(a: LeftLongT, b: LeftLongT): LeftLongT = {
      (a, b) match {
        case (NoLeftLong, _) => NoLeftLong
        case (_, NoLeftLong) => NoLeftLong
        case (LeftLong(x), LeftLong(y)) => LeftLong(range(x, y))
      }
    }
  }
  
  given semiringILeftLong: SemiringI[LeftLongT] with {
    export semiringLeftLong.*
    override def index(i: Int): LeftLongT = LeftLong(Range(i, i))
  }
  
}