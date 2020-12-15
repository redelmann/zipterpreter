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

import Church._

/** Example showcasing the various interpreters for lambda calculus. */
object Main {
  def main(args: Array[String]): Unit = {
    val factDefinition: Value = RecInterpreter(
      e"fix (λ fact n . (if (zero? n) (λ x . x) (λ u . * n (fact (pred n)))) 1)",
      Env.fix ++ Env.prim)

    val envWithFact = Env.prim ++ Env("fact" -> factDefinition)

    VerboseZippyInterpreter(e"fact 5", envWithFact)
  }
}
