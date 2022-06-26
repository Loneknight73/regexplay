package regexplay

import Semiring.*

object MarkedWeightedRegex {

  case class REGW[C, S](emptyw: S, finalw: S, regw: RE[C, S])
  //  {
  //    override def toString: String = "[" + emptyw + "," + finalw + "," + regw.toString + "]"
  //  }

  sealed trait RE[C, S]

  case class EPSW[C, S]() extends RE[C, S]

  case class SYMW[C, S](f: C => S) extends RE[C, S]

  case class ALTW[C, S](a: REGW[C, S], b: REGW[C, S]) extends RE[C, S]

  case class SEQW[C, S](a: REGW[C, S], b: REGW[C, S]) extends RE[C, S]

  case class REPW[C, S](a: REGW[C, S]) extends RE[C, S]

  // Smart constructors
  def epsw[C, S: Semiring]: REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(ev.one, ev.zero, EPSW())
  }

  def symw[C, S: Semiring](f: C => S): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(ev.zero, ev.zero, SYMW(f))
  }

  def altw[C, S: Semiring](p: REGW[C, S], q: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(ev.add(p.emptyw, q.emptyw),
      ev.add(p.finalw, q.finalw),
      ALTW(p, q))
  }

  def seqw[C, S: Semiring](p: REGW[C, S], q: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(ev.mult(p.emptyw, q.emptyw),
      ev.add(ev.mult(p.finalw, q.emptyw), q.finalw),
      SEQW(p, q))
  }

  def repw[C, S: Semiring](r: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(ev.one, r.finalw, REPW(r))
  }

  def matchw[C, S: Semiring](r: REGW[C, S], s: List[C]): S = {
    val ev = summon[Semiring[S]]
    s match {
      case Nil => r.emptyw
      case c :: cs => cs.foldLeft(shiftw(ev.one, r.regw, c))
        ((r, c) => shiftw(ev.zero, r.regw, c)).finalw
    }
  }

  def shiftw[C, S: Semiring](m: S, re: RE[C, S], c: C): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    re match {
      case EPSW() => epsw
      case SYMW(f) => symw(f).copy(finalw = ev.mult(m, f(c)))
      case ALTW(p, q) => altw(shiftw(m, p.regw, c), shiftw(m, q.regw, c))
      case SEQW(p, q) => seqw(shiftw(m, p.regw, c),
        shiftw(ev.add(ev.mult(m, p.emptyw), p.finalw), q.regw, c))
      case REPW(r) => repw(shiftw(ev.add(m, r.finalw), r.regw, c))
    }
  }

  def submatchw[C, S: Semiring](r: REGW[(Int, C), S], s: List[C]): S = {
    val ev = summon[Semiring[S]]
    val arb: REGW[(Int, C), S] = repw(symw(_ => ev.one))
    matchw(seqw(arb, seqw(r, arb)), (s.indices zip s).toList)
  }

  ////////////////////////////////////////////////////////////

  def symi[S: SemiringI](c: Char): REGW[(Int, Char), S] = {
    val sri = summon[SemiringI[S]]
    val sr = summon[Semiring[S]]

    def weight(t: (Int, Char)): S = {
      val (pos, x) = t
      if (x == c) sri.index(pos)
      else sr.zero
    }
    symw(weight)
  }
}

object MainMarkedWeightedRegex extends App {

    import MarkedWeightedRegex._
    import Semiring._
    import Semiring.semiringILeftmost

    val a = symi('a')
    val ab = repw(altw(a, symi('b')))
    val aaba = seqw(a, seqw (ab, a))

    println(submatchw(a, "bbbbaa".toList))
    println(submatchw(aaba, "ab".toList))
    println(submatchw(aaba, "aa".toList))
    println(submatchw(aaba, "bababa".toList))
}

object MainMarkedWeightedRegex2 extends App {

    import MarkedWeightedRegex._
    import Semiring._
    import Semiring.semiringILeftLong

    val a = symi('a')
    val ab = repw(altw(a, symi('b')))
    val aaba = seqw(a, seqw (ab, a))

    println(submatchw(a, "bbbbaa".toList))
    println(submatchw(aaba, "ab".toList))
    println(submatchw(aaba, "aa".toList))
    println(submatchw(aaba, "bababa".toList))
}

object MainMarkedWeightedRegex3 extends App {

    import MarkedWeightedRegex._
    import Semiring._
    import Semiring.semiringBool

    val ev = summon[Semiring[Boolean]]
    val a = symw(_ == 'a')

    def seqn[C](n: Int)(t: REGW[C, Boolean]): REGW[C, Boolean] = {
      val xs = List.fill(n)(t) 
      xs.reduceRight((x, y) => seqw[C, Boolean](x, y))
    }

    def re(n: Int) = seqw(seqn(n) (altw(a, epsw)), seqn(n)(a))
    val m = Utils.time(matchw(re(500), List.fill(500)('a')))
    println(m)
}


