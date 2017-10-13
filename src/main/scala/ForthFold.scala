package forthFold
import ForthError.ForthError
import cats.Foldable
import cats.data.State
import cats.implicits._ // because cats built on 2.11 which didn't have Monad[Either]

package object forthFold {
  type Program = List[Word]
  type Stack = List[Int]
  type ForthErrorOr[A] = Either[ForthError, A]
  type Result = ForthErrorOr[ForthState]

  case class ForthState(stack: Stack,
                        forthFunctions: Map[String, (Stack => ForthErrorOr[Stack])]) extends ForthEvaluatorState {
    override def toString: String = stack.reverse.mkString(" ")
  }
}

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}


sealed trait Word
case class Definition(name: String, cmd: Seq[Word]) extends Word
case class Number(value: Int) extends Word
case class Func(cmd: String) extends Word


trait ForthEvaluator  {
  def eval(text: String): Either[ForthError, ForthEvaluatorState]
}
trait ForthEvaluatorState {
  override def toString: String
}
object Forth {
  def parse(in: String): List[Word] = {
    // regex help from github.com/daewon
    val BeforeDefn = """(.*?)(?=:)(.*)""".r
    val tokens = """(-?\d+)|([\p{L}\p{Sc}\-]+)|(\s[-+*/])|^[-+*/]""".r
    val Defn = """(?s)\s*(:.+?;)(.*)""".r
    val Nmbr = """(\d+)""".r
    val FnSeq = """([\p{L}\p{Sc}\-]+)""".r
    val BinOp = """([-+*/]{1})""".r
    val Symb = """[\p{L}\p{Sc}\-]+""".r

    def parseForTokens(token: String): List[String] = token match {
      case Defn(userDefined, after) => userDefined :: parseForTokens(after)
      case BeforeDefn(before, after) => parseForTokens(before) ++ parseForTokens(after) // in case Defns are not first
      case seq => tokens.findAllMatchIn(seq).flatMap(_.subgroups).filterNot(_ == null).toList    }

    val parsed = parseForTokens(in.toUpperCase)

    parsed.map(_.trim).map {
      case Defn(userDefined, _) =>
        val Array(name, cmd@_*) = userDefined.trim.tail.init.split(" ").filter(_.trim.nonEmpty)
        if (Symb.unapplySeq(name).isEmpty) throw new IllegalArgumentException("userDefined name is Number")
        else Definition(name, parse(cmd.mkString(" ")))

      case Nmbr(n) => Number(n.toInt)
      case FnSeq(cmd) => Func(cmd)
      case BinOp(op) => Func(op)
      case ex@_ => throw new RuntimeException(s"Unsupported Word: '${ex}'")
    }.toList
  } // end def parse
}

class Forth extends ForthEvaluator {
  import forthFold._
  import Forth._

  type Result = ForthErrorOr[ForthState]
  val startFunctions: Map[String, (Stack => ForthErrorOr[Stack])] = {

//    def scalaOp(f: (Int => Int => Int))(s:Stack): Either[ForthError, Stack] = s match {
//      case _ :: Nil            => Left(ForthError.StackUnderflow)
//      case x :: y :: _ => push(f(x)(y)) (s)
//    } don't think this will work in scala bc of methods

    Map("+" -> {
        case s:Stack if s.length < 2 =>       Left(ForthError.StackUnderflow)
        case x :: y :: z => Right((x+y) :: z)
        case _ =>  Left(ForthError.StackUnderflow)
            },
      "-" -> {
        case s:Stack  if s.length < 2 =>       Left(ForthError.StackUnderflow)
        case x :: y :: z => Right((y-x) :: z)
        case _ =>  Left(ForthError.StackUnderflow)
      },
      "/" -> {
        case s:Stack  if s.length < 2 =>       Left(ForthError.StackUnderflow)
        case x :: _ :: _  if x == 0 => Left(ForthError.DivisionByZero)
        case x :: y :: z          => Right((y/x) :: z)
        case _ =>  Left(ForthError.StackUnderflow)
      },
      "*" -> {
        case s:Stack  if s.length < 2 =>       Left(ForthError.StackUnderflow)
        case x :: y :: z          => Right((y/x) :: z)
        case _ =>  Left(ForthError.StackUnderflow)
      },
      "DUP" -> {
        case Nil            =>  Left(ForthError.StackUnderflow)
        case x :: y => Right(x :: x :: y)
      },
      "DROP" -> {
        case Nil            =>  Left(ForthError.StackUnderflow)
        case _ :: y => Right(y)
      },
      "SWAP" -> {
        case s:Stack  if s.length < 2 =>       Left(ForthError.StackUnderflow)
        case x :: y :: z => Right(y :: x :: z)
      },
      "OVER" -> {
        case s:Stack  if s.length < 2 =>       Left(ForthError.StackUnderflow)
        case x :: y :: _ => Right(y :: x :: y)
      }
    )

  }
  val startState = new ForthState(List(), startFunctions)

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = // two-steps
    evalParsed(parse(text), startState)

    // for{ parsed <- parse(text) yield evalParsed
  def parse(text: String): List[Word] = Forth.parse(text) // in companion obj // TODO parse returns Either monad

  def evalWord(state: ForthState, w: Word): Result = {
    def callFunction(name: String, state: ForthState): Result = {
      def oldStackToNewState(st: Stack) = Right(ForthState(st, state.forthFunctions))
      state.forthFunctions.get(name) match {
        case None => Left(ForthError.UnknownWord) // wish print the unknownword here
        case Some(f) => f(state.stack).flatMap(oldStackToNewState)
      }
    }

    // now use callFunction
    val stk = state.stack
    val fns = state.forthFunctions
    w match {
      case Func(name) => callFunction(name, state)
      case Number(n) => Right(ForthState(n :: stk, fns))
      case Definition(name, cmd) => {
        def updatedStackResultingFromFunction(s:Stack): ForthErrorOr[Stack] = evalParsed(cmd.toList, ForthState(s, fns)).map {
          case ForthState(stack_of_functionResult, _) => stack_of_functionResult // map case in order to unwrap Either
        }
        Right(ForthState(stk, state.forthFunctions.updated(name, updatedStackResultingFromFunction )))
//        def oldFnsToNewState(st: Stack) = Right(ForthState(stk, state.forthFunctions.updated(name, updatedStackResultingFromFunction )))
//
//        updatedStackResultingFromFunction.flatMap(oldStackToNewState)

      } // end case Definition()


    } //end match
  } // end evalword


  def evalParsed(words: List[Word], state: ForthState): Result = {
    cats.Foldable[List].foldLeftM(words, startState)(evalWord) // need cats monadic foldleft for the wrap it in a monad because i want it to actually operate, not just use the monoid instance (not sure what that is anyway)
  }

}
