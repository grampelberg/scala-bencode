/* Copyright (C) 2009 Thomas Rampelberg <pyronicide@gmail.com>

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

/* Take a scala object and bencode it.
 */

def int(input: Int): String =
  "i" + input + "e"

def string(input: String): String =
  input.length + ":" + input

def list(input: List[_]): String =
  "l" + input.map( x => dispatcher(x)).mkString + "e"

def dictionary(input: Map[String, _]): String =
  "d" + input.map(
    x => (x._1, dispatcher(x._2))).flatMap( x => x._1 + x._2 ).mkString + "e"

def dispatcher(input: Any): String =
  input match {
    case x: Int => int(x)
    case x: String => string(x)
    case x: List[_] => list(x)
    case x: Map[String, _] => dictionary(x)
    case _ => ""
  }

