package regexplay

import scala.math.min

object Semiring {

  trait Semiring[S] {
    def zero: S

    def one: S

    def add(a: S, b: S): S

    def mult(a: S, b: S): S
  }

  def sum[S](l: List[S])(implicit sr: Semiring[S]): S = {
    l.foldLeft(sr.zero)((a, b) => sr.add(a, b))
  }

  def prod[S](l: List[S])(implicit sr: Semiring[S]): S = {
    l.foldLeft(sr.one)((a, b) => sr.mult(a, b))
  }

  implicit val semiRingBool = new Semiring[Boolean] {
    def zero = false

    def one = true

    override def add(a: Boolean, b: Boolean): Boolean = {
      a || b
    }

    override def mult(a: Boolean, b: Boolean): Boolean = {
      a && b
    }
  }

  implicit val semiRingInt = new Semiring[Int] {
    def zero = 0

    def one = 1

    override def add(a: Int, b: Int): Int = {
      a + b
    }

    override def mult(a: Int, b: Int): Int = {
      a * b
    }
  }

  trait SemiringI[S] extends Semiring[S] {
    def index: Int => S
  }

  /////////////////////// Leftmost /////////////////////

  sealed trait LeftmostT
  case object NoLeft extends LeftmostT
  case class Leftmost(s: StartT) extends LeftmostT

  sealed trait StartT
  case object NoStart extends StartT
  case class Start(pos: Int) extends StartT

  class semiringLeftmostC extends Semiring[LeftmostT] {

    def zero = NoLeft
    def one  = Leftmost(NoStart)

    override def add(a: LeftmostT, b: LeftmostT): LeftmostT = {
      (a, b) match {
        case (NoLeft, x) => x
        case (x, NoLeft) => x
        case (Leftmost(x), Leftmost(y)) => {
          def leftmost(xx: StartT, yy: StartT): StartT = {
            (xx, yy) match {
              case (NoStart, NoStart) => NoStart
              case (NoStart, Start(i)) => Start(i)
              case (Start(i), NoStart) => Start(i)
              case (Start(i), Start(j)) => Start(min(i, j))
            }
          }

          Leftmost(leftmost(x, y))
        }
      }
    }

    override def mult(a: LeftmostT, b: LeftmostT): LeftmostT = {
      (a, b) match {
        case (NoLeft, _) => NoLeft
        case (_, NoLeft) => NoLeft
        case (Leftmost(x), Leftmost(y)) => {
          def start(xx: StartT, yy: StartT): StartT = {
            (xx, yy) match {
              case (NoStart, s) => s
              case (s,       _) => s
            }
          }
          Leftmost(start(x, y))
        }
      }
    }
  }

  implicit val semiringLeftmost = new semiringLeftmostC()

  class semiringILeftmostC extends semiringLeftmostC with SemiringI[LeftmostT] {
    def index = (i: Int) =>
      Leftmost(Start(i))
  }

  implicit val semiringILeftmost = new semiringILeftmostC()

}

object MainSemi extends App {

  import Semiring._

  val l = List(1, 2, 3, 4)
  var x = prod(l)
  println(x)
  x = sum(l)
  println(x)
}