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

import scala.annotation._

/** Stack of layers, which record the *context*
  * in which expressions or values appear.
  */
type Context = List[Layer]

/** Layer of context.
  *
  * Each layer corresponds to a recursive
  * expression position in the `Expr` datatype.
  */
sealed trait Layer

/** Indicates that the current focus is on the left
  * of an application, and that `arg` is the right argument.
  */
case class InAppLeft(arg: Expr) extends Layer

/** Indicates that the current focus is on the right
  * of an application, and that `fun` is the function.
  *
  * Note that `fun` is a `Value` as in order to move the
  * focus towards the right of an application the left
  * must already have been evaluated.
  */
case class InAppRight(fun: Value) extends Layer

/** Indicates that the current focus is within the body of
  * an abstraction, which happens when a closure is evaluated.
  *
  * The argument `env` records the previous environment which is
  * replaced by the closure's environment.
  */
case class InAbs(env: Env) extends Layer

/** A focus in a given context and environment.
  *
  * The type of focus is left abstract.
  * In practice, this will generarally be an expression or
  */
case class Focused[A](focus: A, context: Context, env: Env)


/** Zipper-based interpreter for lambda calulus expressions. */
object ZippyInterpreter {

  /** Performs a step of evaluation.
    *
    * Each step starts with an unevaluated expression as the focus,
    * with all expressions the left of that expression having been evaluated.
    *
    * In a first phase, the focus is moved down the expression
    * towards the smallest left-most unevalualted expression.
    * Then, the expression is turned into a value.
    *
    * In a second phase, the focus is moved up the context,
    * applying layers until either the context is empty of
    * the next unevaluated expression is found.
    */
  def step(expr: Focused[Expr]): Either[Value, Focused[Expr]] =
    up(down(expr))

  /** Applies an unbounded number of evaluation `step`s
    * on a focused expression until hopefully a value is returned.
    *
    * This method is not garanteed to terminate.
    */
  @tailrec
  def eval(expr: Focused[Expr]): Value = step(expr) match {
    case Left(value) => value
    case Right(newExpr) => eval(newExpr)
  }

  /** Moves the focus down towards the smallest left-most unevaluated expression
    * and evaluates it, leaving it in the same context.
    */
  @tailrec
  def down(state: Focused[Expr]): Focused[Value] = state.focus match {
    case Var(name) =>
      Focused(state.env(name), state.context, state.env)
    case App(fun, arg) =>
      down(Focused(fun, InAppLeft(arg) :: state.context, state.env))
    case abs@Abs(_, _) =>
      Focused(Closure(state.env, abs), state.context, state.env)
    case prim@Prim(_) =>
      Focused(prim, state.context, state.env)
  }

  /** Moves the focus up towards the first left-most unevaluated expression. */
  @tailrec
  def up(state: Focused[Value]): Either[Value, Focused[Expr]] = state.context match {
    case Nil =>
      Left(state.focus)
    case InAppLeft(arg) :: rest =>
      Right(Focused(arg, InAppRight(state.focus) :: rest, state.env))
    case InAppRight(Closure(env, Abs(name, body))) :: rest =>
      Right(Focused(body, InAbs(state.env) :: rest, env + (name -> state.focus)))
    case InAppRight(Prim(fun)) :: rest => {
      val Prim(arg) = state.focus
      val casted = fun.asInstanceOf[Any => Any]
      up(Focused(Prim(casted(arg)), rest, state.env))
    }
    case InAbs(env) :: rest =>
      up(Focused(state.focus, rest, env))
  }

  /** Evaluates an expression in a given environment
    * using a zipper-based method.
    */
  def apply(expr: Expr, env: Env = Map.empty): Value =
    eval(Focused(expr, Nil, env))
}

/** Simple recursive interpreter for lambda calculus expressions. */
object RecInterpreter {

  /** Evaluates an expression in a given environment
    * using the typical recursive interpretion method.
    */
  def apply(expr: Expr, env: Env = Map.empty): Value = expr match {
    case Var(x) => env(x)
    case abs@Abs(_, _) => Closure(env, abs)
    case App(fun, arg) => {
      val v1 = apply(fun, env)
      val v2 = apply(arg, env)
      v1 match {
        case Closure(newEnv, Abs(name, body)) => {
          apply(body, newEnv + (name -> v2))
        }
        case Prim(fun) => {
          val Prim(arg) = v2
          val casted = fun.asInstanceOf[Any => Any]
          Prim(casted(arg))
        }
      }
    }
    case prim@Prim(_) => prim
  }
}