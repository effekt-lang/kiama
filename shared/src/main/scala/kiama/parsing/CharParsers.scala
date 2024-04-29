package kiama
package parsing

import kiama.util.Positions
import kiama.util.Source

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

class CharParsers(positions: Positions) extends Parsers(positions) {
  type Token = Char
  type In = SourceInput
  
  /**
   * Run a parser on a string to obtain its result.
   */
  def parse[T](p: Parser[T], source: Source): ParseResult[T] =
    p(SourceInput(source, 0))

  /**
   * Run a parser on all of a string to obtain its result.
   */
  def parseAll[T](p: Parser[T], source: Source): ParseResult[T] =
    parse(phrase(p), source)
  
  
  /**
   * A parser that matches a literal string after skipping any whitespace.
   * The form of the latter is defined by the `whitespace` parser.
   */
  implicit def literal(s: String): Parser[String] =
    Parser {
      in =>
        if (in.source.content.regionMatches(in.offset, s, 0, s.length)) {
          Success(s, SourceInput(in.source, in.offset + 1))
        } else {
          Failure(s"'$s' expected but ${in.found} found'", in)
        }
    }
}

class RegexParsers(positions: Positions) extends CharParsers(positions) {

  /**
   * A parser that matches a regex string after skipping any whitespace.
   * The form of the latter is defined by the `whitespace` parser.
   */
  def regex(r: Regex, expected: => String): Parser[String] =
    Parser {
      in =>
        val s = in.source.content.substring(in.offset)
        r.findPrefixMatchOf(s) match {
          case Some(m) =>
            Success(s.substring(0, m.end), SourceInput(in.source, in.offset + m.end))
          case None =>
            Failure(s"${expected} expected but ${in.found} found", in)
        }
    }


  implicit def regex(r: Regex): Parser[String] =
    regex(r, s"string matching regex '$r'")

  /**
   * A parser that matches a regex string after skipping any whitespace.
   * The form of the latter is defined by the `whitespace` parser.
   */
  def matchRegex(r: Regex): Parser[Match] =
    Parser {
      in =>
        val s = in.source.content.substring(in.offset)
        r.findPrefixMatchOf(s) match {
          case Some(m) =>
            Success(m, SourceInput(in.source, in.offset + m.end))
          case None =>
            Failure(s"string matching regex '$r' expected but ${in.found} found", in)
        }
    }

  /**
   * Parse digit strings that are constrained to fit into an `Int` value.
   * If the digit string is too big, a parse error results.
   */
  lazy val constrainedInt: Parser[Int] =
    wrap(regex("[0-9]+".r), stringToInt)

  /**
   * Parser for keywords. The list of string arguments gives the text
   * of the keywords in a language. The regular expression gives the
   * possible extension of the keyword to stop the keyword being seen as
   * an identifier instead. For example, the keyword list might contain
   * `"begin"` and `"end"` and the extension regular expression might
   * be `[^a-zA-Z0-9]`. Thus, `begin` followed by something other than
   * a letter or digit is a keyword, but `beginfoo8` is an identifier.
   * This parser succeeds if any of the keywords is present, provided
   * that it's not immediately followed by something that extends it.
   */
  def keywords(ext: Regex, kws: List[String]): Parser[String] =
    regex("(%s)(%s|\\z)".format(kws.mkString("|"), ext).r, "Keyword")
}