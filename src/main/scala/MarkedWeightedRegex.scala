package regexplay

import regexplay.Semiring.{Semiring, SemiringI}

object MarkedWeightedRegex {

  case class REGW[C, S](emptyw: S, finalw: S, regw: RE[C, S]) {
    override def toString: String = "[" + emptyw + "," + finalw + "," + regw.toString + "]"
  }

  sealed trait RE[C, S]

  case class EPSW[C, S]() extends RE[C, S]

  case class SYMW[C, S](f: C => S) extends RE[C, S]

  case class ALTW[C, S](a: REGW[C, S], b: REGW[C, S]) extends RE[C, S]

  case class SEQW[C, S](a: REGW[C, S], b: REGW[C, S]) extends RE[C, S]

  case class REPW[C, S](a: REGW[C, S]) extends RE[C, S]

  // Smart constructors
  def epsw[C, S](implicit ev: Semiring[S]): REGW[C, S] = {
    REGW(ev.one, ev.zero, EPSW())
  }

  def symw[C, S](f: C => S)(implicit ev: Semiring[S]): REGW[C, S] = {
    REGW(ev.zero, ev.zero, SYMW(f))
  }

  def altw[C, S](p: REGW[C, S], q: REGW[C, S])(implicit ev: Semiring[S]): REGW[C, S] = {
    REGW(ev.add(p.emptyw, q.emptyw),
      ev.add(p.finalw, q.finalw),
      ALTW(p, q))
  }

  def seqw[C, S](p: REGW[C, S], q: REGW[C, S])(implicit ev: Semiring[S]): REGW[C, S] = {
    REGW(ev.mult(p.emptyw, q.emptyw),
      ev.add(ev.mult(p.finalw, q.emptyw), q.finalw),
      SEQW(p, q))
  }

  def repw[C, S](r: REGW[C, S])(implicit ev: Semiring[S]): REGW[C, S] = {
    REGW(ev.one, r.finalw, REPW(r))
  }

  def shiftw[C, S](m: S, re: RE[C, S], c: C)(implicit ev: Semiring[S]): REGW[C, S] = {
    re match {
      case EPSW()     => epsw
      case SYMW(f)    => symw(f).copy(finalw = ev.mult(m, f(c)))
      case ALTW(p, q) => altw(shiftw(m, p.regw, c), shiftw(m, q.regw, c))
      case SEQW(p, q) => seqw(shiftw(m, p.regw, c), shiftw(ev.add(ev.mult(m, p.emptyw), p.finalw), q.regw, c))
      case REPW(r)    => repw(shiftw(ev.add(m, r.finalw), r.regw, c))
    }
  }

  def shiftwhelper[C, S] (implicit ev: Semiring[S]): (REGW[C, S], C) => REGW[C, S] = {
    (r, c) => shiftw(ev.zero, r.regw, c)
  }

  def matchw[C, S] (r: REGW[C, S], s: List[C])(implicit ev: Semiring[S]): S = {
    s match {
      case List() => r.emptyw
      case c :: cs => (cs.foldLeft[REGW[C,S]]
        (shiftw(ev.one, r.regw, c))
        (shiftwhelper[C, S]))
        .finalw
    }
  }

  def submatchw[C, S] (r: REGW[(Int, C), S], s: List[C])(implicit ev: Semiring[S]): S = {
    val arb: REGW[(Int, C), S] = repw(symw(_ => ev.one))
    matchw(seqw(arb, seqw(r, arb)), (s.indices zip s).toList)
  }

  ////////////////////////////////////////////////////////////

  def symi[S](c: Char)(implicit ev: SemiringI[S]): REGW[(Int, Char), S] = {
    def weight: ((Int, Char)) => S = {
      t => {
        val pos = t._1
        val x = t._2
        if (x == c) ev.index(pos)
        else ev.zero
      }
    }
    symw[(Int, Char), S](weight)
  }

}

object MainMarkedWeightedRegex extends App {

  import MarkedWeightedRegex._
  import Semiring._

  val a = symi[LeftmostT]('a')
  val ab = repw(altw(a, symi[LeftmostT]('b')))
  val aaba = seqw(a, seqw (ab, a))

  var r: LeftmostT = _
  r = submatchw[Char, LeftmostT](a, "bbbbaa".toList)
  println(r)
  r = submatchw[Char, LeftmostT](aaba, "ab".toList)
  println(r)
  r =  submatchw[Char, LeftmostT](aaba, "aa".toList)
  println(r)
  r =  submatchw[Char, LeftmostT](aaba, "bababa".toList)
  println(r)
  r = submatchw[Char, LeftmostT](a, "a".toList)
  println(r)

}