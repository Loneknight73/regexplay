package regexplay

object LazySemiring {

  trait Semiring[S] {
    def zero: S
    def one: S
    def add(a: => S, b: => S): S
    def mult(a: => S, b: => S): S
  }

  given lazySemiringBool: Semiring[Boolean] with {
    def zero = false

    def one = true

    def add(a: => Boolean, b: => Boolean): Boolean = a || b

    def mult(a: => Boolean, b: => Boolean): Boolean = a && b
  }

}

import LazySemiring.*

object MarkedWeightedRegexInf {

  case class REGW[C, S](active: Boolean, emptyw: S, finalw: S, reg: () => REG[C, S])

  sealed trait REG[C, S]

  case class EPS[C, S]() extends REG[C, S]

  case class SYM[C, S](f: C => S) extends REG[C, S]

  case class ALT[C, S](a: () => REGW[C, S], b: () => REGW[C, S]) extends REG[C, S]

  case class SEQ[C, S](a: () => REGW[C, S], b: () => REGW[C, S]) extends REG[C, S]

  case class REP[C, S](a: () => REGW[C, S]) extends REG[C, S]

  // Smart constructors
  def epsw[C, S: Semiring]: REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false, ev.one, ev.zero, () => EPS())
  }
  
  def symw[C, S: Semiring](f: C => S): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false, ev.zero, ev.zero, () => SYM(f))
  }

  def altw[C, S: Semiring](p: REGW[C, S], q: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(p.active || q.active,
      ev.add(p.emptyw, q.emptyw),
      ev.add(p.finalw, q.finalw),
      () => ALT(() => p, () => q))
  }

  def seqw[C, S: Semiring](p: REGW[C, S], q: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(p.active || q.active,
      ev.mult(p.emptyw, q.emptyw),
      ev.add(ev.mult(finala(p), q.emptyw), finala(q)),
      () => SEQ(() => p, () => q))
  }

  def repw[C, S: Semiring](r: REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(r.active, ev.one, r.finalw, () => REP(() => r))
  }

  // Additional special constructors setting weights to zero
  def seq[C, S: Semiring] (p: => REGW[C, S], q: => REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false,
      ev.mult(p.emptyw, q.emptyw),
      ev.zero,
      () => SEQ(() => p, () => q))
  }

  def alt[C, S: Semiring] (p: => REGW[C, S], q: => REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false,
      ev.add(p.emptyw, q.emptyw),
      ev.zero,
      () => ALT(() => p, () => q))
  }

  def rep[C, S: Semiring] (r: => REGW[C, S]): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    REGW(false, ev.one, ev.zero, () => REP(() => r))
  }

  def finala[C, S: Semiring](r: => REGW[C, S]): S = {
    val ev = summon[Semiring[S]]
    if (r.active) r.finalw else ev.zero
  }

  def matchw[C, S: Semiring](r: () => REGW[C, S], s: List[C]): S = {
    val ev = summon[Semiring[S]]
    s match {
      case Nil => r().emptyw
      case c :: cs => cs.foldLeft(shiftw(ev.one, r, c))
        ((r, c) => shiftw(ev.zero, () => r, c)).finalw
    }
  }

  def shiftw[C, S: Semiring](m: S, r: () => REGW[C, S], c: C): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    if (r().active || m != ev.zero) stepw(m, r().reg, c) else r()
  }

  def stepw[C, S: Semiring](m: S, re: () => REG[C, S], c: C): REGW[C, S] = {
    val ev = summon[Semiring[S]]
    re() match {
      case EPS() => epsw
      case SYM(f) => {
        val fin = ev.mult(m, f(c))
        symw(f).copy(active = (fin != ev.zero), finalw = fin)
      }
      case ALT(p, q) => altw(shiftw(m, p, c), shiftw(m, q, c))
      case SEQ(p, q) => seqw(shiftw(m, p, c),
        shiftw(ev.add(ev.mult(m, p().emptyw), p().finalw), q, c))
      case REP(r) => repw(shiftw(ev.add(m, r().finalw), r, c))
    }
  }

  def submatchw[C, S: Semiring](r: () => REGW[(Int, C), S], s: List[C]): S = {
    val ev = summon[Semiring[S]]
    val arb: REGW[(Int, C), S] = repw(symw(_ => ev.one))
    matchw(() => seqw(arb, seqw(r(), arb)), (s.indices zip s).toList)
  }

}

object MainInfRegex extends App {
  import MarkedWeightedRegexInf._
  import LazySemiring.*


  val a: REGW[Char, Boolean] = symw(_ == 'a')
  val b: REGW[Char, Boolean] = symw(_ == 'b')
  val anbn: REGW[Char, Boolean] = alt(epsw, seq(a, seq(anbn, b)))
 
  println(matchw(() => anbn, "".toList))
  println(matchw(() => anbn, "ab".toList))
  println(matchw(() => anbn, "aabb".toList))
  println(matchw(() => anbn, "aabbb".toList))

}

object MainInfRegex2 extends App {
  import MarkedWeightedRegexInf._
  import LazySemiring.*

  val a: REGW[Char, Boolean] = symw(_ == 'a')
  def bs(n: Int): List[REGW[Char, Boolean]] = List.fill(n)(symw(_ == 'b'))
  def cs(n: Int): List[REGW[Char, Boolean]] = List.fill(n)(symw(_ == 'c'))

  def bcs(n: Int): REGW[Char, Boolean] = {
    val bcsn = bs(n) ++ cs(n)
    bcsn.reduceRight((x, y) => seq[Char, Boolean](x, y))
  }

  def abc(n: Int): REGW[Char, Boolean] = seq(a, alt(bcs(n), abc(n+1)))
  val anbncn = alt(epsw, abc(1))

  println(matchw(() => anbncn, "".toList))
  println(matchw(() => anbncn, "abc".toList))
  println(matchw(() => anbncn, "aabbcc".toList))
  println(matchw(() => anbncn, "aabbbcc".toList))
}