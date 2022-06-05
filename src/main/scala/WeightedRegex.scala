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

  def sym[S: Semiring](c: Char): Regw[Char, S] = {
    val sr = summon[Semiring[S]]
    Symw(x => if (x == c) sr.one else sr.zero)
  }

  def weighted[S: Semiring](r: Reg): Regw[Char, S] = {
    r match {
      case Eps       => Epsw()
      case Sym(c)    => sym(c)
      case Alt(p,q)  => Altw(weighted(p), weighted(q))
      case Seq(p,q)  => Seqw(weighted(p), weighted(q))
      case Rep(p)    => Repw(weighted(p))
    }
  }

  def acceptw[C, S: Semiring](regex: Regw[C, S], u: List[C]): S = {
    val sr = summon[Semiring[S]]
    regex match {
      case Epsw()  => if (u == Nil) sr.one else sr.zero
      case Symw(f) => u match {
        case c :: Nil => f(c)
        case _ => sr.zero
      }
      case Altw(p,q) => sr.add(acceptw(p, u), acceptw(q, u))
      case Seqw(p,q) =>
        sum(for {
          (u1, u2) <- split(u)
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
  import Semiring.{semiringBool,semiringInt}

  def as = Alt(Sym('a'), Rep(Sym('a')))
  def bs =  Alt(Sym('b'), Rep(Sym('b')))
  def seqabs = Seq(as, bs)
  println(acceptw(weighted(as)(semiringInt), "a".toList))
  println(acceptw(weighted(seqabs)(semiringInt), "ab".toList))
  println(acceptw(weighted(seqabs)(semiringInt), "abc".toList))
  println(acceptw(weighted(seqabs)(semiringBool), "aaabbbbbbb".toList))
  println(acceptw(weighted(seqabs)(semiringBool), "aaacbbbbbbb".toList))
  println(acceptw(weighted(Rep(Eps))(semiringInt), "".toList))
}

