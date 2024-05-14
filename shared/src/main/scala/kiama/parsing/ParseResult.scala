package kiama
package parsing

/**
 * Parse results.
 */
sealed abstract class ParseResult[In <: Input[_], +Out] {

  def kind: String

  //def toMessage: String

  def next: In

  def append[Out1 >: Out](r: => ParseResult[In, Out1]): ParseResult[In, Out1]

  def flatMapWithNext[Out1](f: Out => In => ParseResult[In, Out1]): ParseResult[In, Out1]

  def map[Out1](f: Out => Out1): ParseResult[In, Out1]

}

/**
 * A successful parse result.
 */
case class Success[In <: Input[_], +Out](result: Out, next: In) extends ParseResult[In, Out] {

  val kind = "success"

  def append[Out1 >: Out](r: => ParseResult[In, Out1]): ParseResult[In, Out1] =
    this

  def flatMapWithNext[Out1](f: Out => In => ParseResult[In, Out1]): ParseResult[In, Out1] =
    f(result)(next)

  def map[Out1](f: Out => Out1): ParseResult[In, Out1] = {
    val u = f(result)
    Success(u, next)
  }

}

/**
 * All parse results that are not successful.
 */
sealed abstract class NoSuccess[In <: Input[_]](val message: String, val next: In) extends ParseResult[In, Nothing] {

  def flatMapWithNext[Out](f: Nothing => In => ParseResult[In, Out]): ParseResult[In, Out] =
    this

  def map[Out](f: Nothing => Out): ParseResult[In, Out] =
    this

}

/**
 * Support for NoSuccess.
 */
object NoSuccess {

  def unapply[In <: Input[_], Out](r: ParseResult[In, Out]): Option[(String, In)] =
    r match {
      case Error(m, n) => Some((m, n))
      case Failure(m, n) => Some((m, n))
      case _ => None
    }

}

/**
 * An error parse result. Parsers that error do not backtrack.
 */
case class Error[In <: Input[_]](override val message: String, override val next: In) extends NoSuccess(message, next) {

  val kind = "error"

  def append[Out >: Nothing](r: => ParseResult[In, Out]): ParseResult[In, Out] =
    this

}

/**
 * A failure parse result.
 */
case class Failure[In <: Input[_]](override val message: String, override val next: In) extends NoSuccess(message, next) {

  val kind = "failure"

  def append[Out >: Nothing](r: => ParseResult[In, Out]): ParseResult[In, Out] = {
    val rr = r
    rr match {
      case _: NoSuccess[In] =>
        if (rr.next.offset < next.offset)
          this
        else
          rr
      case _ =>
        rr
    }
  }

}
