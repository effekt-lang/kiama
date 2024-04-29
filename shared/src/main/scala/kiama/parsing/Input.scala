/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package parsing

import kiama.util.{Position, Source}

enum Token {
  case Space
  case Keyword(s: String)
}

/**
 * The input consumed by a parser.
 */
trait Input[+Token] {

  import kiama.util.Position

  /**
   * Are we at the end of the input?
   */
  def atEnd: Boolean

  /**
   * The first character of the input if we are not at the end.
   */
  def first: Option[Token]

  /**
   * The rest of the input, unchanged if already at end.
   */
  def rest: Input[Token]
  
  def offset: Int
}

case class SourceInput(source: Source, offset: Int) extends Input[Char] {

  def atEnd: Boolean =
    source.content.length <= offset

  def first: Option[Char] =
    if (atEnd) None else Some(source.content.charAt(offset))

  def rest: SourceInput =
    if (atEnd) this else SourceInput(source, offset + 1)

  /**
   * Return the current position of the input.
   */
  val position: Position = source.offsetToPosition(offset)

  /**
   * Return the next position of the input.
   */
  val nextPosition: Position = source.offsetToPosition(offset + 1)

  /**
   * Return a formatted description of this input.
   */
  def format: String =
    s"${found} (${position.line},${position.column})"

  /**
   * Return a description of the current character found in the input,
   * either the actual character is there is one, or "end of source" if
   * we are at the end.
   */
  def found: String =
    if (offset == source.content.length)
      "end of source"
    else
      s"'${source.content.charAt(offset)}'"
}

case class TokenInput[Token](tokens: Seq[Token], offset: Int) extends Input[Token] {

  val atEnd: Boolean =
    tokens.length <= offset

  def first: Option[Token] =
    if (atEnd) None else Some(tokens(offset))

  def rest: Input[Token] =
    if (atEnd) this else TokenInput(tokens, offset + 1)
}