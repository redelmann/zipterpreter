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

/** Environment, mapping variables to their value. */
type Env = Map[String, Value]

import scala.language.implicitConversions

/** Contains various environments. */
object Env {

  /** Builds an environment. */
  def apply(pairs: (String, Value)*): Env =
    Map(pairs: _*)

  /** Empty environment. */
  def empty: Env = Map.empty

  /** Environment with primitive definitions. */
  val prim: Env = Map(
    "succ" -> Primitives.succ,
    "pred" -> Primitives.pred,
    "+" -> Primitives.plus,
    "-" -> Primitives.minus,
    "*" -> Primitives.mult,
    "^" -> Primitives.exp,
    "true" -> Primitives.tru,
    "false" -> Primitives.fls,
    "if" -> Primitives.ifThenElse,
    "and" -> Primitives.and,
    "or" -> Primitives.or,
    "not" -> Primitives.not,
    "zero?" -> Primitives.isZero,
    "<=" -> Primitives.lessEq,
    "=" -> Primitives.equal,
  )

  /** Environment with Church-encoded definitions. */
  val church: Env = Map(
    "succ" -> Closure(Church.succ),
    "pred" -> Closure(Church.pred),
    "+" -> Closure(Church.plus),
    "-" -> Closure(Church.minus),
    "*" -> Closure(Church.mult),
    "^" -> Closure(Church.exp),
    "true" -> Closure(Church.tru),
    "false" -> Closure(Church.fls),
    "if" -> Closure(Church.ifThenElse),
    "and" -> Closure(Church.and),
    "or" -> Closure(Church.or),
    "not" -> Closure(Church.not),
    "zero?" -> Closure(Church.isZero),
    "<=" -> Closure(Church.lessEq),
    "=" -> Closure(Church.equal),
  )

  /** Environment with fixpoint combinator. */
  val fix: Env = Map(
    "fix" -> Closure("f" -:
      ("x" -: "f" ~ ("v" -: "x" ~ "x" ~ "v")) ~
      ("x" -: "f" ~ ("v" -: "x" ~ "x" ~ "v")))
  )
}

/** Contains primitive operations on `BigInt`s. */
object Primitives {

  /** Primitive successor function on `BigInt`s. */
  val succ: Prim = mkFun("SUCC") {
    case Prim(n: BigInt) => Prim(n + 1)
  }

  /** Primitive successor function on `BigInt`s. */
  val pred: Prim = mkFun("PRED") {
    case Prim(n: BigInt) => Prim(n - 1)
  }

  /** Primitive addition function on `BigInt`s. */
  val plus: Prim = mkFun("PLUS") {
    case Prim(x: BigInt) => mkFun("PLUS(" + x + ")") {
      case Prim(y: BigInt) => Prim(x + y)
    }
  }

  /** Primitive subtraction function on `BigInt`s. */
  val minus: Prim = mkFun("MINUS") {
    case Prim(x: BigInt) => mkFun("MINUS(" + x + ")") {
      case Prim(y: BigInt) => Prim(x - y)
    }
  }

  /** Primitive multiplication function on `BigInt`s. */
  val mult: Prim = mkFun("MULT") {
    case Prim(x: BigInt) => mkFun("MULT(" + x + ")") {
      case Prim(y: BigInt) => Prim(x * y)
    }
  }

  /** Primitive exponentiation function on `BigInt`s. */
  val exp: Prim = mkFun("EXP") {
    case Prim(x: BigInt) => mkFun("EXP(" + x + ")") {
      case Prim(y: BigInt) => Prim(x.pow(y.toInt))
    }
  }

  /** Primitive true. */
  val tru: Prim = Prim(true)

  /** Primitive false. */
  val fls: Prim = Prim(false)

  /** Primitive if function. */
  val ifThenElse: Prim = mkFun("IF") {
    case Prim(c: Boolean) => mkFun("IF(" + c + ")") {
      case t => mkFun("IF(" + c + ", ... )") {
        case e => if (c) t else e
      }
    }
  }

  /** Primitive negation. */
  val not: Prim = mkFun("NOT") {
    case Prim(b: Boolean) => Prim(!b)
  }

  /** Primitive conjunction. */
  val and: Prim = mkFun("AND") {
    case Prim(x: Boolean) => mkFun("AND(" + x + ")") {
      case Prim(y: Boolean) => Prim(x && y)
    }
  }

  /** Primitive disjunction. */
  val or: Prim = mkFun("OR") {
    case Prim(x: Boolean) => mkFun("OR(" + x + ")") {
      case Prim(y: Boolean) => Prim(x || y)
    }
  }

  /** Primitive zero-check. */
  val isZero: Prim = mkFun("ISZERO") {
    case Prim(b: BigInt) => Prim(b == BigInt(0))
  }

  /** Primitive less or equal. */
  val lessEq: Prim = mkFun("LEQ") {
    case Prim(x: BigInt) => mkFun("LEQ(" + x + ")") {
      case Prim(y: BigInt) => Prim(x <= y)
    }
  }

  /** Primitive equal. */
  val equal: Prim = mkFun("EQ") {
    case Prim(x) => mkFun("EQ(" + x + ")") {
      case Prim(y) => Prim(x == y)
    }
  }
}

/** Contains the Church encoding of natural numbers and related operations. */
object Church {

  /** Church-encoded `0`. */
  val zero: Abs = "f" -: "x" -: "x"

  /** Church-encoded successor. */
  val succ: Abs = "n" -: "f" -: "x" -: "f" ~ ("n" ~ "f" ~ "x")

  /** Church-encoded successor. */
  val pred: Abs = "n" -: "f" -: "x" -:
    "n" ~ ("g" -: "h" -: "h" ~ ("g" ~ "f")) ~ ("u" -: "x") ~ ("u" -: "u")

  /** Church-encoded addition. */
  val plus: Abs = "m" -: "n" -: "f" -: "x" -: "m" ~ "f" ~ ("n" ~ "f" ~ "x")

  /** Church-encoded subtraction. */
  val minus: Abs = "m" -: "n" -: "n" ~ pred ~ "m"

  /** Church-encoded multiplication. */
  val mult: Abs = "m" -: "n" -: "f" -: "x" -: "m" ~ ("n" ~ "f") ~ "x"

  /** Church-encoded exponentiation. */
  val exp: Abs = "m" -: "n" -: "n" ~ "m"

  /** Church-encoded true. */
  val tru: Abs = "t" -: "f" -: "t"

  /** Church-encoded false. */
  val fls: Abs = "t" -: "f" -: "f"

  /** Church-encoded if. */
  val ifThenElse: Abs = "c" -: "t" -: "e" -: "c" ~ "t" ~ "e"

  /** Church-encoded negation. */
  val not: Abs = "c" -: "t" -: "f" -: "c" ~ "f" ~ "t"

  /** Church-encoded conjunction. */
  val and: Abs = "p" -: "q" -: "p" ~ "q" ~ "p"

  /** Church-encoded disjunction. */
  val or: Abs = "p" -: "q" -: "p" ~ "p" ~ "q"

  /** Church-encoded zero-check. */
  val isZero: Abs = "n" -: "n" ~ ("x" -: fls) ~ tru

  /** Church-encoded less or equal. */
  val lessEq: Abs = "m" -: "n" -: isZero ~ (minus ~ "m" ~ "n")

  /** Church-encoded equality on integers. */
  val equal: Abs = "m" -: "n" -: and ~ (lessEq ~ "m" ~ "n") ~ (lessEq ~ "n" ~ "m")


  /** Returns the Church-encoded natural number literal corresponding to `n`. */
  implicit def lit(n: Int): Abs = "f" -: "x" -: (0 until n).foldLeft(variable("x")) {
    case (acc, _) => "f" ~ acc
  }

  /** Turns a Church-encoded number into a primitive number. */
  def decodeInt(encoded: Expr): Expr =
    encoded ~ Primitives.succ ~ Prim(BigInt(0))

  /** Turns a Church-encoded boolean into a primitive boolean. */
  def decodeBool(encoded: Expr): Expr =
    encoded ~ Prim(true) ~ Prim(false)
}