package regexplay

object WeightedRegex {

  import SimpleRegex._
  import Semiring._
  import ListUtils._

  sealed trait Regw[C, S]

  case class Epsw[C, S]()                             extends Regw[C, S]
  case class Symw[C, S] (f: C => S)                   extends Regw[C, S]
  case class Altw[C, S](a: Regw[C, S], b: Regw[C, S]) extends Regw[C, S]
  case class Seqw[C, S](a: Regw[C, S], b: Regw[C, S]) extends Regw[C, S]
  case class Repw[C, S](a: Regw[C, S])                extends Regw[C, S]

  def sym[S](c: Char)(implicit sr: Semiring[S]): Regw[Char, S] = {
    Symw(x => if (x == c) sr.one else sr.zero)
  }

  def weighted[S](r: Reg)(implicit sr: Semiring[S]): Regw[Char, S] = {
    r match {
      case Eps       => Epsw()
      case Sym(c)    => sym(c)
      case Alt(p,q)  => Altw[Char, S](weighted[S](p), weighted[S](q))
      case Seq(p,q)  => Seqw[Char, S](weighted[S](p), weighted[S](q))
      case Rep(p)    => Repw[Char, S](weighted[S](p))
    }
  }

  def acceptw[C, S](regex: Regw[C, S], u: List[C])(implicit sr: Semiring[S]): S = {
    regex match {
      case Epsw()  => if (u == List()) sr.one else sr.zero
      case Symw(f) => u match {
        case c :: Nil => f(c)
        case _ => sr.zero
      }
      case Altw(p,q) => sr.add(acceptw(p, u), acceptw(q, u))
      case Seqw(p,q) =>
        sum(for {
          (u1, u2) <- split(u.toList)
        } yield sr.mult(acceptw(p,u1), acceptw(q,u2)))
      case Repw(r) =>
        sum(for {
          ps <- parts(u)
          x = prod(for {
            ui <- ps
          } yield acceptw(r, ui))
        } yield x)
    }
  }

}

object MainWeighted extends App {

  import SimpleRegex._
  import WeightedRegex._
  import Semiring._

  def as = Alt(Sym('a'), Rep(Sym('a')))
  def bs =  Alt(Sym('b'), Rep(Sym('b')))
  def seqabs = Seq(as, bs)
  var p: Int = acceptw[Char, Int](weighted(as), "a".toList)
  println(p)
  p = acceptw[Char, Int](weighted(seqabs), "ab".toList)
  println(p)
  var b = acceptw[Char, Boolean](weighted(seqabs), "abc".toList)
  println(b)
  b = acceptw[Char, Boolean](weighted(seqabs), "aaabbbbbbb".toList)
  println(b)

}

