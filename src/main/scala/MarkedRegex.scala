package regexplay

object MarkedRegex {

  sealed trait REG
  case object EPS extends REG
  case class SYM(m: Boolean, c: Char) extends REG
  case class ALT(a: REG, b: REG) extends REG
  case class SEQ(a: REG, b: REG) extends REG
  case class REP(a: REG) extends REG

  def empty(regex: REG): Boolean = {
    regex match {
      case EPS       => true
      case SYM(_, _) => false
      case ALT(p, q) => empty(p) || empty(q)
      case SEQ(p, q) => empty(p) && empty(q)
      case REP(r)    => true
    }
  }

  def hasfinal(regex: REG): Boolean = {
    regex match {
      case EPS       => false
      case SYM(b, _) => b
      case ALT(p, q) => hasfinal(p) || hasfinal(q)
      case SEQ(p, q) => (hasfinal(p) && empty(q)) || hasfinal(q)
      case REP(r)    => hasfinal(r)
    }
  }

  def shift(m: Boolean, regex: REG, c: Char): REG = {
    regex match {
      case EPS       => EPS
      case SYM(_, x) => SYM(m && (x == c), x)
      case ALT(p, q) => ALT(shift(m, p, c), shift(m, q, c))
      case SEQ(p, q) => SEQ(shift(m, p, c),
                            shift(m && empty(p) || hasfinal(p), q, c))
      case REP(r)    => REP(shift(m || hasfinal(r), r, c))
    }
  }

  def rmatch(r: REG, s: String): Boolean = {
    s.toList match {
      case Nil  => empty(r)
      case c :: cs => hasfinal(cs.foldLeft(shift(true, r, c))(shift(false, _, _)))
    }
  }
}

object MainMarkedRegex extends App {
  import MarkedRegex._

  def nocs = REP(ALT(SYM(false, 'a'), SYM(false, 'b')))
  def onec = SEQ(nocs, SYM(false, 'c'))
  def evencs = SEQ(REP(SEQ(onec, onec)), nocs)

  var p = rmatch(evencs, "abbabba")
  println(p)
  p = rmatch(evencs, "acc")
  println(p)
  p = rmatch(evencs, "accc")
  println(p)
  p = rmatch(evencs, "caccc")
  println(p)
}
