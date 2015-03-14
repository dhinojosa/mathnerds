trait Eq[a] {
  def eq(a1: a)(a2: a): Boolean
}

sealed abstract class GameResult
case object Win extends GameResult
case object Loss extends GameResult
case object Tie extends GameResult

implicit object GameResultEq extends Eq[GameResult] {
  def eq(a1: GameResult)(a2: GameResult) = (a1, a2) match {
    case (Win, Win) => true
    case (Loss, Loss) => true
    case (Tie, Tie) => true
    case _ => false
  }
}

def newIsEquals(x: GameResult, y: GameResult)(implicit eq: Eq[GameResult]) = {
  eq.eq(x)(y)
}

println(newIsEquals(Win, Win))