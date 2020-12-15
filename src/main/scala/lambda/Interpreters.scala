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
case class Focused[+A](focus: A, context: Context, env: Env)


/** A zipper-based interpreter for lambda calulus expressions.
  *
  * @param verbose Indicates if a trace should be printed to
  *                standard output.
  */
class ZippyInterpreter(val verbose: Boolean) {

  /** If `verbose`, prints a line to standard output. */
  private def display(text: => String): Unit =
    if (verbose) println(text)

  /** Performs a step of evaluation.
    *
    * Each step starts with an unevaluated expression as the focus,
    * with all expressions the left of that expression having been evaluated.
    *
    * In a first phase, the focus is moved down the expression
    * towards the smallest left-most unevalualted expression.
    *
    * In a second phase, that basic expression is turned into a value.
    *
    * In a third and final phase phase, the focus is moved up the context,
    * applying layers until either the context is empty of
    * the next unevaluated expression is found.
    */
  def step(expr: Focused[Expr]): Either[Value, Focused[Expr]] =
    up(replace(down(expr)))

  /** Applies an unbounded number of evaluation `step`s
    * on a focused expression until hopefully a value is returned.
    *
    * This method is not garanteed to terminate.
    */
  @tailrec
  final def eval(expr: Focused[Expr]): Value = step(expr) match {
    case Left(value) => value
    case Right(newExpr) => eval(newExpr)
  }

  /** Moves the focus down towards the smallest left-most unevaluated expression. */
  @tailrec
  final def down(state: Focused[Expr]): Focused[Basic] = stepDown(state) match {
    case Left(newState) => {
      display("\n======= DOWN =======\n")
      display(Display(newState))
      down(newState)
    }
    case Right(newState) =>
      newState
  }

  /** Moves the focus downwards one step.
    *
    * Results with the focus either on an expression which must be downwards visited,
    * or on a basic expression which can directly be turned in a value.
    */
  def stepDown(state: Focused[Expr]): Either[Focused[Expr], Focused[Basic]] = state.focus match {
    case basic: Basic =>
      Right(Focused(basic, state.context, state.env))
    case App(fun, arg) =>
      Left(Focused(fun, InAppLeft(arg) :: state.context, state.env))
  }

  /** Replaces the basic expression in focus with the corresponding value. */
  def replace(state: Focused[Basic]): Focused[Value] = {
    val newState = Focused(state.focus.eval(state.env), state.context, state.env)
    display("\n======= REP. =======\n")
    display(Display(newState))
    newState
  }

  /** Moves the focus up towards the first left-most unevaluated expression. */
  @tailrec
  final def up(state: Focused[Value]): Either[Value, Focused[Expr]] = stepUp(state) match {
    case None =>
      Left(state.focus)
    case Some(Left(newState)) => {
      display("\n======== UP ========\n")
      display(Display(newState))
      up(newState)
    }
    case Some(Right(newState)) =>
      display("\n======== UP ========\n")
      display(Display(newState))
      Right(newState)
  }

  /** Moves the focus upwards one step.
    *
    * A result of `None` indicates that the value is within an empty context.
    * Otherwise, results with the focus either on a value which must still be propagated upwards,
    * or on an expression which can should later be downwards visited.
    */
  def stepUp(state: Focused[Value]): Option[Either[Focused[Value], Focused[Expr]]] =
    state.context match {
      case Nil =>
        None
      case InAppLeft(arg) :: rest =>
        Some(Right(Focused(arg, InAppRight(state.focus) :: rest, state.env)))
      case InAppRight(Closure(env, Abs(name, body))) :: rest =>
        Some(Right(Focused(body, InAbs(state.env) :: rest, env + (name -> state.focus))))
      case InAppRight(Prim(fun)) :: rest => {
        val casted = fun.asInstanceOf[Value => Value]
        Some(Left(Focused(casted(state.focus), rest, state.env)))
      }
      case InAbs(env) :: rest =>
        Some(Left(Focused(state.focus, rest, env)))
    }

  /** Evaluates an expression in a given environment
    * using a zipper-based method.
    */
  def apply(expr: Expr, env: Env = Map.empty): Value = {
    val state = Focused(expr, Nil, env)
    display("\n======= START ======\n")
    display(Display(state))
    val result = eval(state)
    display("\n====== RESULT ======\n")
    display(Display(result))
    result
  }
}

/** A zipper-based interpreter for lambda calulus expressions.
  *
  * The interpreter is not verbose.
  */
object ZippyInterpreter extends ZippyInterpreter(false)

/** A verbose zipper-based interpreter for lambda calulus expressions. */
object VerboseZippyInterpreter extends ZippyInterpreter(true)

/** Simple recursive interpreter for lambda calculus expressions. */
object RecInterpreter {

  /** Evaluates an expression in a given environment
    * using the typical recursive interpretion method.
    */
  def apply(expr: Expr, env: Env = Map.empty): Value = expr match {
    case basic: Basic => basic.eval(env)
    case App(fun, arg) => {
      val v1 = apply(fun, env)
      val v2 = apply(arg, env)
      v1 match {
        case Closure(newEnv, Abs(name, body)) => {
          apply(body, newEnv + (name -> v2))
        }
        case Prim(fun) => {
          val casted = fun.asInstanceOf[Value => Value]
          casted(v2)
        }
      }
    }
  }
}