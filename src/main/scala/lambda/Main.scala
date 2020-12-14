/* Copyright 2020 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package lambda

import scala.language.implicitConversions

/** Contains the Church encoding of natural numbers and related operations. */
object Church {

  /** Church-encoded `0`. */
  val zero: Expr = "f" -: "x" -: "x"

  /** Church-encoded successor. */
  val succ: Expr = "n" -: "f" -: "x" -: "f" ~ ("n" ~ "f" ~ "x")

  /** Church-encoded addition. */
  val plus: Expr = "m" -: "n" -: "f" -: "x" -: "m" ~ "f" ~ ("n" ~ "f" ~ "x")

  /** Church-encoded multiplication. */
  val mult: Expr = "m" -: "n" -: "f" -: "x" -: "m" ~ ("n" ~ "f") ~ "x"

  /** Church-encoded exponentiation. */
  val exp: Expr = "m" -: "n" -: "n" ~ "m"

  /** Returns the Church-encoded natural number literal corresponding to `n`. */
  implicit def lit(n: Int): Expr = "f" -: "x" -: (0 until n).foldLeft(variable("x")) {
    case (acc, _) => "f" ~ acc
  }

  /** Turns a Church-encoded number into a primitive number. */
  def decode(encoded: Expr): Expr =
    encoded ~ Prim((x: Int) => x + 1) ~ Prim(0)
}

import Church._

/** Example showcasing the various interpreters for lambda calculus. */
object Main {
  def main(args: Array[String]): Unit = {
    val value: Int = 34 + ((17 + 1) * (1 << 3))
    val encoded: Expr = plus ~ 34 ~ (mult ~ (succ ~ 17) ~ (exp ~ 2 ~ 3))
    val decoded: Expr = decode(encoded)
    println("Ground truth: " + value)
    println("Zippy interpreter: " + ZippyInterpreter(decoded))
    println("Recursive interpreter: " + RecInterpreter(decoded))
  }
}
