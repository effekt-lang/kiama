package kiama.parsing

import kiama.util.{ Positions, StringSource }

import munit.Location

enum Token {
  case Integer(n: Int)
  case `+`
  case `(`
  case `)`
}

enum Exp {
  case Lit(n: Int)
  case Add(l: Exp, r: Exp)
}

class SimpleParsers(positions: Positions) extends Parsers(positions) {

  type Elem = Token

  import Token.*

  lazy val exp: P[Exp] =
    ( exp ~ (`+` ~> atomic) ^^ { case l ~ r => Exp.Add(l, r) }
    | atomic
    )

  lazy val atomic: P[Exp] =
    ( `(` ~> exp <~ `)`
    | num
    )

  lazy val num: P[Exp] = accept("Number literal") { case Token.Integer(n) => Exp.Lit(n) }

  implicit def tok2Parser(t: Token): Parser[Token] = elem(t)

}

class ParsersTests extends munit.FunSuite {

  def parseExp(tokens: List[Token]): ParseResult[Input[Token], Exp] =
    val positions = Positions()
    val p = new  SimpleParsers(positions)
    p.parseAll(p.exp, TokenInput(tokens, 0, StringSource("..."), tok => 0))

  def assertSuccess(tokens: List[Token], expected: Exp)(using Location): Unit =
    parseExp(tokens) match {
      case Success(result, next) => assertEquals(expected, result)
      case failure: Failure[_] => fail(s"Expected ${expected}, but got error: ${failure.message}")
      case error: Error[_] => fail(s"Expected ${expected}, but got error: ${error.message}")
    }

  def assertFailure(tokens: List[Token])(using Location): Unit =
    parseExp(tokens) match {
      case Success(result, next) => fail(s"Expected error, but got: ${result}")
      case failure: Failure[_] => ()
      case error: Error[_] => ()
    }

  test("simple parsing") {
    import Token.*

    assertSuccess(List(Integer(42)), Exp.Lit(42))
    assertFailure(List(`+`))

    assertSuccess(List(`(`, Integer(42), `)`), Exp.Lit(42))
    assertFailure(List(`(`, Integer(42)))

    assertSuccess(List(`(`, Integer(42), `)`, `+`, Integer(3)), Exp.Add(Exp.Lit(42), Exp.Lit(3)))
  }
}
