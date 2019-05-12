package regexplay

object ListUtils {

  def or(xs: List[Boolean]): Boolean = {
    xs.foldLeft(false)( (x, acc) => x || acc)
  }

  def and(xs: List[Boolean]): Boolean = {
    xs.foldLeft(true)( (x, acc) => x && acc)
  }

  def split[A](a: List[A]): List[(List[A], List[A])] = {
    a match {
      case List() => List((List(), List()))
      case c :: cs => {
        val l = for {
          (s1, s2) <- split(cs)
        } yield (c :: s1, s2)
        (List(), c :: cs) :: l
      }
    }
  }

  def parts[A](a: List[A]): List[List[List[A]]] = {
    a match {
      case List() => List(List())
      case c :: Nil => List(List(List(c)))
      case c :: cs => {
        val r =  for {
          (p :: ps) <- parts(cs)
        } yield List((c :: p) :: ps, List(c) :: p :: ps)
        r.flatten
      }
    }
  }
}

object MainListUtils extends App {
  import ListUtils._

  var pacc = parts("acc".toList)
  println("[" + pacc.map(s => "[" + (s.map(l => "\"" + l.mkString + "\"").mkString(",")) + "]").mkString(",") + "]")
  println("*" * 50)
  var sacc = split("acc".toList)
  sacc.map(println(_))
}