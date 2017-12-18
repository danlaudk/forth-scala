package forth
import ForthError.ForthError
import cats.{Eval, Foldable}
import cats.data.State
import cats.implicits._ // because cats built on 2.11 which didn't have Monad[Either]

package object forth {
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
    val Defn = """^(?s)\s*(:.+?;)(.*)""".r
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
    }
  } // end def parse
}

class Forth extends ForthEvaluator {
  import forth._
  import Forth._
  type Result = ForthErrorOr[ForthState]
  val startFunctions: Map[String, (Stack => Either[ForthError,Stack])] = {

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
        case x :: y :: z          => Right((y*x) :: z)
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
        case x :: y :: z => Right(y :: x :: y :: z)
      }
    )

  }
  val startState = new ForthState(List(), startFunctions)

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    // two-steps without statemonad. 3-steps with. evalStateActions(parse(text) map wordToState, Right(startState)) // originally a 2-step version
    evalToState(parse(text)).runS(Right(startState)).value
  }
  def parse(text: String): List[Word] = Forth.parse(text) // in companion obj // TODO parse returns Either monad

  def evalToState(words: Seq[Word]): State[Result, Eval[Result]] =
    if(words.length==0)
      for {
        r <- State.get[Result]
      } yield Eval.later(r)
    else {
      for {
        r <- State.get[Result]
        (stk, fns) = r match { case Right(ForthState(stk, fns)) => (stk, fns) }
        _ <- words.head match {
          case Func(name) => State.set[Result](callFunctionIntoResult(fnInMap(name)(r))(r))
          case Number(n) => State.set[Result](Right(ForthState(n :: stk, fns)))
          case Definition(name, cmd) => State.set[Result](Right(ForthState(stk, fns.updated(name, updatedStackResultingFromFunction(fns)(cmd)(_).value))))
        }
        res <- evalToState(words.tail)
      } yield res
    }

  // fns used in wordtoState and in evalProgram
  type Map_Functions = Map[String, (Stack => Either[ForthError,Stack])]
  def fnInMap(name: String)(r: Result): Option[Stack => Either[ForthError,Stack]] = r.toOption.get.forthFunctions.get(name)

  def callFunctionIntoResult(fn: Option[Stack => Either[ForthError,Stack]])(r: Result): Result = {
    def oldStackToNewState(st: Stack) = Right(ForthState(st, r.toOption.get.forthFunctions))

    fn match {
      case None => Left(ForthError.UnknownWord)
      case Some(f) => {
        r match {
          case Right(state) => f(state.stack).flatMap(oldStackToNewState _)
          case Left(x) => Left(x)
        }
      }
    }
  }// callfunctionintoresult
  def updatedStackResultingFromFunction(currentFunctions: Map_Functions)(cmd: Seq[Word])(s:Stack): Eval[Either[ForthError,Stack]] =
    evalToState(cmd).runS(Right(ForthState(s, currentFunctions))).map{_ match {case Right(ForthState(stack_of_functionResult, _)) => Right(stack_of_functionResult)}}

}
