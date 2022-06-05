package regexplay

object SimpleRegex {

  import ListUtils._

  sealed trait Reg

  case object Eps extends Reg
  case class Sym(c: Char) extends Reg
  case class Alt(a: Reg, b: Reg) extends Reg
  case class Seq(a: Reg, b: Reg) extends Reg
  case class Rep(a: Reg) extends Reg

  def accept(regex: Reg, u: String): Boolean = {
    regex match {
      case Eps => u == ""
      case Sym(c) => u == c.toString
      case Alt(p, q) => accept(p, u) || accept(q, u)
      case Seq(p: Reg, q: Reg) =>
        val lb = for {
          (u1, u2) <- split(u.toList)
          b = accept(p, u1.mkString) && accept(q, u2.mkString)
        } yield b
        or(lb)
      case Rep(r) =>
        val lb = for {
          ps <- parts(u.toList)
          x = and(for {
            ui <- ps
          } yield accept(r, ui.mkString))
        } yield x
        or(lb)
    }
  }
}

object MainSimple extends App {

  import SimpleRegex._

  def nocs = Rep(Alt(Sym('a'), Sym('b')))
  def onec = Seq(nocs, Sym('c'))
  def evencs = Seq(Rep(Seq(onec, onec)), nocs)

  var p = accept(evencs, "abbabba")
  println(p)
  println(accept(nocs, "abbabba"))
  p = accept(evencs, "acc")
  println(p)
  println("accept(evencs, \"acc\"): " + accept(evencs, "acc"))
  p = accept(evencs, "accc")
  println(p)
  p = accept(evencs, "caccc")
  println(p)
}

