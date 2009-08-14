/* The lexer that is used by the bencode parser.
 *
 * Copyright (C) 2009 Thomas Rampelberg <pyronicide@gmail.com>

 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.

 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place - Suite 330, Boston, MA 02111-1307, USA.
 */

package org.saunter.bencode

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

class Lexer extends StdLexical {
  /**
   * The token grammar for bencode. Both strings and integers are returned as
   * whole tokens. Dictionaries and lists are handled by the parser and the
   * 'l', 'd', 'e' characters are converted into keywords for consumption by
   * the parser.
   */
  override def token: Parser[Token] =
    ( number ^^ NumericLit
    | string ^^ processIdent
    | dstart ^^ Keyword
    | lstart ^^ Keyword
    | expr_end ^^ Keyword )

  // Integer: i10e | i-10e
  def number = 'i' ~> signed_int <~ 'e'
  def signed_int = opt('-') ~ rep1(digit) ^^ {
    case x ~ y => (optString("", x) :: y) mkString "" }

  // Shamelessly ripped from scala.util.parsing.json.Lexer
  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  // String: length:string -> 3:foo
  def string = len >> { x => repN(x.toInt, char) ^^ {
    case y => y mkString "" } }
  def len = rep1(digit) <~ ':' ^^ { case x => x mkString "" }
  // Any char ..... not sure if this is actually valid or not. It's valid based
  // on the BNF grammar for bencoding at least.
  def char = elem("char", x => true)

  // Keywords for start/end of lists and dictionaries
  def dstart = elem("dictionary start", d => d == 'd') ^^ {
    case x => x toString }
  def lstart = elem("list start", d => d == 'l') ^^ { case x => x toString }
  def expr_end = elem("expression end", d => d == 'e') ^^ {
    case x => x toString }
}
