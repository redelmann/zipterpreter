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

  /** Application of `this` to `that`.
    *
    * The method is left-associative, meaning that
    * `f ~ a ~ b` results in `App(App(f, a), b)`.
    *
    */
  def ~(that: Expr): Expr = App(this, that)

  /** Abstraction of `this` by `param`.
    *
    * Note that, as the method end with a ':',
    * the `param` should appear to the *left* of `this`.
    * Additionally, the method is right associative,
    * meaning that `x -: y -: body` results in
    * `Abs(x, Abs(y, body))`.
    */
  def -:(param: String): Expr = Abs(param, this)
}

/** Variable. */
case class Var(name: String) extends Expr

/** Implicit conversion from strings to variables. */
implicit def variable(name: String): Expr = Var(name)

/** Application of `fun` to `arg`. */
case class App(fun: Expr, arg: Expr) extends Expr

/** Abstraction by `param` of `body`. */
case class Abs(param: String, body: Expr) extends Expr

/** Primitive value.
  *
  * Note that it is both an expression and a value.
  * The `value` argument may be a (Scala) function,
  * in which case it is can be used as
  */
case class Prim(value: Any) extends Expr with Value

/** Result of a computation. */
sealed trait Value

/** Abstraction with an environment of captured variables. */
case class Closure(env: Env, abs: Abs) extends Value

/** Environment, mapping variables to their value. */
type Env = Map[String, Value]

/** Syntactic sugar for let expressions. */
def let(name: String, value: Expr, body: Expr): Expr =
  (name -: body) ~ value