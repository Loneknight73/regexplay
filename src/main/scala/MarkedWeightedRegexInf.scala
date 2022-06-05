package regexplay

import Semiring.*

object MarkedWeightedRegexInf {

  case class REGW[C, S](active: Boolean, emptyw: S, finalw: S, reg: REG[C, S])

  sealed trait REG[C, S]

  case class EPS[C, S]() extends REG[C, S]

  case class SYM[C, S](f: C => S) extends REG[C, S]

  case class ALT[C, S](a: () => REGW[C, S], b: () => REGW[C, S]) extends REG[C, S]

  case class SEQ[C, S](a: REGW[C, S], b: REGW[C, S]) extends REG[C, S]

  case class REP[C, S](a: REGW[C, S]) extends REG[C, S]

  // Smart constructors
  def epsw[C, S: Semiring]: REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false, ev.one, ev.zero, EPS())
  }

  def symw[C, S: Semiring](f: C => S): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false, ev.zero, ev.zero, SYM(f))
  }

  def altw[C, S: Semiring](p: REGW[C, S], q: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(p.active || q.active,
      ev.add(p.emptyw, q.emptyw),
      ev.add(p.finalw, q.finalw),
      ALT(p, q))
  }

  def seqw[C, S: Semiring](p: REGW[C, S], q: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(p.active || q.active,
      ev.mult(p.emptyw, q.emptyw),
      ev.add(ev.mult(finala(p), q.emptyw), finala(q)),
      SEQ(p, q))
  }

  def repw[C, S: Semiring](r: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(r.active, ev.one, r.finalw, REP(r))
  }

  // Additional special constructors setting weights to zero
  def seq[C, S: Semiring] (p: => REGW[C, S], q: => REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false,
      ev.mult(p.emptyw, q.emptyw),
      ev.zero,
      SEQ(p, q))
  }

  def alt[C, S: Semiring] (p: => REGW[C, S], q: => REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false,
      ev.add(p.emptyw, q.emptyw),
      ev.zero,
      ALT(p, q))
  }
  
  def rep[C, S: Semiring] (r: => REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false, ev.one, ev.zero, REP(r))
  }

  def finala[C, S: Semiring](r: REGW[C, S]): S = {
    val ev = summon[Semiring[S]]
    if (r.active) r.finalw else ev.zero
  }

  def matchw[C, S: Semiring](r: => REGW[C, S], s: List[C]): S = {
    val ev = summon[Semiring[S]]
    s match {
      case Nil => r.emptyw
      case c :: cs => cs.foldLeft(shiftw(ev.one, r, c))
        ((r, c) => shiftw(ev.zero, r, c)).finalw
    }
  }

  def shiftw[C, S: Semiring](m: S, r: REGW[C, S], c: C): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    if (r.active || m != ev.zero) stepw(m, r.reg, c) else r
  }

  def stepw[C, S: Semiring](m: S, re: REG[C, S], c: C): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    re match {
      case EPS() => epsw
      case SYM(f) => {
        val fin = ev.mult(m, f(c))
        symw(f).copy(active = (fin != ev.zero), finalw = fin)
      }
      case ALT(p, q) => altw(shiftw(m, p, c), shiftw(m, q, c))
      case SEQ(p, q) => seqw(shiftw(m, p, c),
        shiftw(ev.add(ev.mult(m, p.emptyw), p.finalw), q, c))
      case REP(r) => repw(shiftw(ev.add(m, r.finalw), r, c))
    }
  }

  def submatchw[C, S: Semiring](r: REGW[(Int, C), S], s: List[C]): S = {
    val ev = summon[Semiring[S]]
    val arb: REGW[(Int, C), S] = repw(symw(_ => ev.one))
    matchw(seqw(arb, seqw(r, arb)), (s.indices zip s).toList)
  }

}

object MainInfRegex extends App {
  import MarkedWeightedRegexInf._
  import Semiring._
  import Semiring.semiringBool

  val a: REGW[Char, Boolean] = symw(_ == 'a')
  val b: REGW[Char, Boolean] = symw(_ == 'b')
  def anbnrec: REGW[Char, Boolean] = {
    lazy val aux = anbnrec
    lazy val r = alt(epsw, seq(a, seq(aux, b)))
    r
  }
  
  //lazy val anbn = anbnrec
  println(matchw(anbnrec, "".toList))
}