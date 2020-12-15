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

import scallion._
import scallion.util.Unfolds._
import silex._

sealed trait Token

object Token {
  case object LambdaToken extends Token
  case object DotToken extends Token
  case class IdentifierToken(name: String) extends Token
  case class ParenthesisToken(isOpen: Boolean) extends Token
  case class PrimToken(value: Any) extends Token
  case class ExprToken(expr: Expr) extends Token
  case object SpaceToken extends Token
  case class UnknownToken(content: String) extends Token
}
import Token._


object Lexical extends Lexers with CharRegExps {

  type Token = lambda.Token
  type Position = Unit

  val firstIdChar = elem(_.isLetter) | oneOf("+-*_^/:!?$<>=")
  val idChar = firstIdChar | elem(_.isDigit)

  // Description of the lexer.
  val lexer = Lexer(
    // Lambda
    elem('\\') | elem('λ') |> { cs => LambdaToken },

    // Dot
    elem('.') |> { cs => DotToken },

    // Parentheses
    elem('(') |> ParenthesisToken(true),
    elem(')') |> ParenthesisToken(false),

    // Spaces
    many1(whiteSpace) |> SpaceToken,

    // Identifiers
    firstIdChar ~ many(idChar) |>
      { cs => IdentifierToken(cs.mkString) },

    // Literal numbers
    many1(elem(_.isDigit)) |>
      { cs => PrimToken(BigInt(cs.mkString)) }

  ) onError {
    (cs, _) => UnknownToken(cs.mkString)
  }

  def apply(it: Iterator[Char]): Iterator[Token] = {
    val source = Source.fromIterator(it, NoPositioner)

    val tokens = lexer(source)

    tokens.filter((token: Token) => token != SpaceToken)
  }

  def unapply(tokens: Iterator[Token]): String = {
    val ts = tokens.toSeq

    val space: ((Token, Token)) => String = {
      case (IdentifierToken(_), IdentifierToken(_)) => " "
      case (IdentifierToken(_), ParenthesisToken(true)) => " "
      case (ParenthesisToken(false), IdentifierToken(_)) => " "
      case (IdentifierToken(_), PrimToken(_)) => " "
      case (PrimToken(_), IdentifierToken(_)) => " "
      case (PrimToken(_), PrimToken(_)) => " "
      case (PrimToken(_), ParenthesisToken(true)) => " "
      case (ParenthesisToken(false), PrimToken(_)) => " "
      case (ParenthesisToken(false), ParenthesisToken(true)) => " "
      case (DotToken, _) => " "
      case _ => ""
    }

    val spaces = "" +: ts.zip(ts.tail).map(space)

    val strings = ts.map {
      case LambdaToken => "λ"
      case IdentifierToken(n) => n
      case DotToken => "."
      case ParenthesisToken(isOpen) => if (isOpen) "(" else ")"
      case PrimToken(value: BigInt) => value.toString
      case PrimToken(value) => "`" + value.toString + "`"
      case _ => "?"
    }

    spaces.zip(strings).map(x => x._1 + x._2).mkString("")
  }
}

// Token kind. Models groups of tokens equivalent for the parser.
sealed abstract class Kind(text: String) {
  override def toString = text
}
object Kind {
  case object IdentifierKind extends Kind("<id>")
  case object LambdaKind extends Kind("\\")
  case object DotKind extends Kind(".")
  case class ParenthesisKind(isOpen: Boolean) extends Kind(if (isOpen) "(" else ")")
  case object PrimKind extends Kind("<prim>")
  case object ExprKind extends Kind("<expr>")
  case object OtherKind extends Kind("?")
}
import Kind._

// The following object describes the syntax of lambda calculus,
// and provides methods to parse and pretty print expressions.
object Syntactic extends Parsers {

  type Token = lambda.Token  // The type of tokens.
  type Kind = lambda.Kind    // The type of token types.

  import SafeImplicits._

  // Returns the kind of tokens.
  override def getKind(token: Token): Kind = token match {
    case IdentifierToken(_) => IdentifierKind
    case DotToken => DotKind
    case LambdaToken => LambdaKind
    case ParenthesisToken(o) => ParenthesisKind(o)
    case PrimToken(_) => PrimKind
    case ExprToken(_) => ExprKind
    case _ => OtherKind
  }

  // Accept any token of the kind IdentifierKind,
  val name: Syntax[String] = accept(IdentifierKind)({
    // Extract the string from them.
    case IdentifierToken(n) => n
  }, {
    // Generates all tokens which could have led to the string.
    // (In this case, only one.)
    case n => Seq(IdentifierToken(n))
  })

  // Accept any token of the kind LambdaKind, which will always be LambdaToken.
  val lamb: Syntax[Unit] = elem(LambdaKind).unit(LambdaToken)

  // Accept any token of the kind DotKind, which will always be DotToken.
  val dot: Syntax[Unit] = elem(DotKind).unit(DotToken)

  // Accepts an open or a close parenthesis.
  def parens(isOpen: Boolean): Syntax[Unit] =
    elem(ParenthesisKind(isOpen)).unit(ParenthesisToken(isOpen))

  // Open parenthesis.
  val open = parens(true)

  // Close parenthesis.
  val close = parens(false)

  // Turn a name into an expression.
  val variable: Syntax[Expr] = name.map({
    // Turn the string into a variable.
    case n => Var(n)
  }, {
    // Turn the expression into all strings that could have generated it.
    case Var(n) => Seq(n)
    case _ => Seq()
  })

  // Literal primitive value.
  val prim: Syntax[Expr] = accept(PrimKind)({
    case PrimToken(value) => Prim(value)
  }, {
    case Prim(value) => Seq(PrimToken(value))
    case _ => Seq()
  })

  val embed: Syntax[Expr] = accept(ExprKind)({
    case ExprToken(expr) => expr
  }, {
    // We do not want to produce this in pretty printing.
    case _ => Seq()
  })

  // The syntax for expressions, which is the main syntax.
  lazy val expr: Syntax[Expr] = recursive {
    // Accepts either a lambda expression or an application.
    // `appExpr` also includes single basic expressions.
    lambdaExpr | appExpr
  }

  // Basic expressions. Simply a variable or an expression in parenthesis.
  lazy val basic: Syntax[Expr] = variable | prim | embed | open.skip ~ expr ~ close.skip

  // Lambda expression.
  lazy val lambdaExpr: Syntax[Expr] = (lamb.skip ~ many1(name) ~ dot.skip ~ expr).map({
    // Given a sequence of names and the expression body, we create the corresponding lambda.
    case ns ~ e => ns.foldRight(e) {  // We do so by using `foldRight`.
      case (n, acc) => Abs(n, acc)  // Create an `Abs` from the name and body.
    }
  }, {
    // We provide the inverse transformation.
    // Given an expression, we decompose it into all its arguments.
    case acc@Abs(_, _) => {
      // To do so, we simply use `unfoldRight`.
      unfoldRight[String, Expr] {
        case Abs(n, acc) => (n, acc)  // We split the `Abs` into its two components.
      }(acc)
    }
    // If the value is not an `Abs`, we have no inverses.
    case _ => Seq()
  })

  // Application, which consists of a sequence of at least one basic expressions.
  lazy val appExpr: Syntax[Expr] = many1(basic).map({
    // We reduce all expressions into a single one using `reduceLeft`.
    xs => xs.reduceLeft(App(_, _))
  }, {
    // We provide also the inverse operation.
    // We unfold arguments using `unreduceLeft`.
    acc => {
      // We use `unreduceLeft` to unpack the value.
      unreduceLeft[Expr] {
        case App(l, r) => (l, r)  // We split the `App` into its two components.
      }(acc)
    }
  })

  // Create the LL1 parser from the syntax description.
  val parser = Parser(expr)

  // Create the pretty printer from the syntax description.
  val printer = PrettyPrinter(expr)

  // Returns the pretty printed representation of an expression.
  def unapply(value: Expr): Option[String] =
    printer(value).map(Lexical.unapply(_))

  // Parses an expression.
  def apply(tokens: Iterator[Token]): Option[Expr] =
    parser(tokens).getValue

  // Parses an expression.
  def apply(text: String): Option[Expr] =
    parser(Lexical(text.iterator)).getValue
}

implicit class Quasiquoter(private val sc: StringContext) extends AnyVal {

  private def toToken(arg: Any): Token =
    if (arg.isInstanceOf[Expr]) {
      Token.ExprToken(arg.asInstanceOf[Expr])
    }
    else {
      Token.PrimToken(arg)
    }

  private def tokenize(args: Seq[Any]): Iterator[Token] = {
    Lexical(sc.parts.head.iterator) ++ args.zip(sc.parts.tail).flatMap {
      case (arg, part) => Iterator(toToken(arg)) ++ Lexical(part.iterator)
    }
  }

  def e(args: Any*): Expr = {
    Syntactic(tokenize(args)).getOrElse {
      throw new Exception("Invalid syntax.")
    }
  }
}
