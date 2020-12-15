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

/** Contains methods to display the various elements of the zippy interpreter. */
object Display {

  private def showContext(context: Context, env: Env, acc: List[String]): List[String] =
    context match {
      case Nil => "[" + apply(env) + "]" :: acc
      case InAppLeft(arg) :: rest => showContext(rest, env, ("* < " + apply(arg)) :: acc)
      case InAppRight(fun) :: rest => showContext(rest, env, (apply(fun) + " > *") :: acc)
      case InAbs(newEnv) :: rest => showContext(rest, newEnv, ("[" + apply(env) + "]") :: acc)
    }

  /** Displays a focused value. */
  @targetName("applyFocusedValue")
  def apply(focused: Focused[Value]): String =
    showContext(focused.context, focused.env, List(apply(focused.focus))).mkString("\n")

  /** Displays a focused expression. */
  @targetName("applyFocusedExpr")
  def apply(focused: Focused[Expr]): String =
    showContext(focused.context, focused.env, List(apply(focused.focus))).mkString("\n")

  /** Displays an expression. */
  def apply(expr: Expr): String =
    expr.pretty

  /** Displays a value. */
  def apply(value: Value): String = value match {
    case Prim(value) => value.toString
    case Closure(env, expr) => "{ " + apply(expr) + " @ " + apply(env) + " }"
  }

  /** Displays an environment. */
  def apply(env: Env): String =
    env.toSeq.map({ case (name, value) => name + " -> " + apply(value) }).mkString(", ")
}
