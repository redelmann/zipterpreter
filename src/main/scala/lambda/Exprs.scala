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

/** Lambda calcus expression. */
sealed trait Expr {

  /** Pretty representation of the expression. */
  lazy val pretty: String =
    Syntactic.unapply(this).getOrElse("?")

  /** Application of `this` to `that`.
    *
    * The method is left-associative, meaning that
    * `f ~ a ~ b` results in `App(App(f, a), b)`.
    *
    */
  def ~(that: Expr): App = App(this, that)

  /** Abstraction of `this` by `param`.
    *
    * Note that, as the method end with a ':',
    * the `param` should appear to the *left* of `this`.
    * Additionally, the method is right associative,
    * meaning that `x -: y -: body` results in
    * `Abs(x, Abs(y, body))`.
    */
  def -:(param: String): Abs = Abs(param, this)
}

sealed trait Basic extends Expr {
  def eval(env: Env): Value
}

/** Variable. */
case class Var(name: String) extends Expr with Basic {
  override def eval(env: Env): Value =
    env(name)
}

/** Implicit conversion from strings to variables. */
implicit def variable(name: String): Expr = Var(name)

/** Application of `fun` to `arg`. */
case class App(fun: Expr, arg: Expr) extends Expr

/** Abstraction by `param` of `body`. */
case class Abs(param: String, body: Expr) extends Expr with Basic {
  override def eval(env: Env): Value =
    Closure(env, this)
}

/** Primitive value.
  *
  * Note that it is both an expression and a value.
  * The `value` argument may be a (Scala) function,
  * in which case it is can be used as
  */
case class Prim(value: Any) extends Expr with Basic with Value {
  override def eval(env: Env): Value =
    this
}

/** Result of a computation. */
sealed trait Value

/** Abstraction with an environment of captured variables. */
case class Closure(env: Env, abs: Abs) extends Value

/** Factory of closures. */
object Closure {

  /** Builds a closure in the empty environment. */
  def apply(abs: Abs): Closure = {
    Closure(Env.empty, abs)
  }
}

/** Syntactic sugar for let expressions. */
def let(name: String, value: Expr, body: Expr): Expr =
  (name -: body) ~ value

/** Syntactic sugar for lazy if expressions.
  *
  * Assumes an "if" in the environment.
  */
def ifLazy(cond: Expr, thn: Expr, els: Expr): Expr =
  ("if" ~ cond ~ ("u" -: thn) ~ ("u" -: els)) ~ ("x" -: "x")

/** Builds a primitive function with a given name (for display purposes). */
def mkFun(name: String)(fun: PartialFunction[Value, Value]): Prim = Prim(new Function1[Value, Value] {
  override val toString: String = name
  override def apply(x: Value): Value = {
    fun.lift(x).getOrElse {
      throw new Exception("Unsupported argument for primitive function " + name + ".")
    }
  }
})