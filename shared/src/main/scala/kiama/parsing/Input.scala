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

/**
 * The input consumed by a parser.
 */
trait Input[+Token] {

  import kiama.util.Position
  
  def source: Source =
    throw new NoSuchMethodException("not a char based parser")

  def offset: Int

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

  /**
   * Return a description of the current character found in the input,
   * either the actual character is there is one, or "end of source" if
   * we are at the end.
   */
  def found: String =
    if (atEnd || first.isEmpty)
      "end of source"
    else
      s"'${first.get}'"
      
  def position: Position
  
  def nextPosition: Position
}

case class SourceInput(override val source: Source, offset: Int) extends Input[Char] {

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
}

case class TokenInput[Token](tokens: Seq[Token], offset: Int, src: Source, toPos: Token => Position) extends Input[Token] {

  def atEnd: Boolean =
    offset >= tokens.length

  def first: Option[Token] =
    if (atEnd) None else Some(tokens(offset))

  def rest: TokenInput[Token] =
    if (atEnd) this else this.copy(offset = offset + 1)

  val position: Position =
    first.map { toPos }.getOrElse(Position(0, 0, src))

  val nextPosition: Position =
    rest.position
}