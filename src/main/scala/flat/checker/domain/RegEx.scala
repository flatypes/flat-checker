package flat.checker.domain

import com.typesafe.scalalogging.LazyLogging

/** A generic regular expression.
 * Names mirror the definition of a Kleene algebra. */
enum RegEx[A] extends LazyLogging:
  case Zero()
  case One()
  case Lit(symbolSet: A)
  case Plus(left: RegEx[A], right: RegEx[A])
  case Comp(left: RegEx[A], right: RegEx[A])
  case Star(part: RegEx[A])

  def +(that: RegEx[A]): RegEx[A] = (this, that) match
    case (Zero(), r) => r // 0 + r = r
    case (r, Zero()) => r // r + 0 = r
    case (r, One()) if r.nullable => r // (r + 1) + 1 = r
    case (One(), r) if r.nullable => r // 1 + (r + 1) = r
    case (Plus(r1, r2), r) if r == r1 || r == r2 => this // (r1 + r2) + r1 = (r1 + r2)
    case (r1, r2) if r1 == r2 => r1 // r + r = r
    case (r1, r2) => Plus(r1, r2)

  def *(that: RegEx[A]): RegEx[A] = (this, that) match
    case (Zero(), _) | (_, Zero()) => Zero() // 0 * r = r * 0 = 0
    case (One(), r) => r // 1 * r = r
    case (r, One()) => r // r * 1 = r
    case (r1, r2) => Comp(r1, r2)

  def star: RegEx[A] = this match
    case Zero() | One() => One() // 0.star = 1.star = 1
    case r => Star(r)

  def plus: RegEx[A] = this * this.star

  def opt: RegEx[A] = One() + this

  def ^(n: Int): RegEx[A] = n match
    case _ if n < 0 => throw IllegalArgumentException("negative exponent")
    case 0 => One()
    case 1 => this
    case _ => this * (this ^ (n - 1))

  def loop(min: Int, max: Option[Int]): RegEx[A] =
    require(min >= 0)
    max match
      case Some(m) if m < min => throw IllegalArgumentException("max < min")
      case Some(m) => (this ^ min) * RegEx.sum((for k <- 0 to (m - min) yield this ^ k).toList)
      case None => (this ^ min) * this.star

  def isEmpty(using SymbolSet[A]): Boolean = this match
    case Zero() => true
    case One() | Star(_) => false
    case Lit(d) => d.isEmpty
    case Plus(r1, r2) => r1.isEmpty && r2.isEmpty
    case Comp(r1, r2) => r1.isEmpty || r2.isEmpty

  def nonEmpty(using SymbolSet[A]): Boolean = !isEmpty

  def nullable: Boolean = this match
    case Zero() | Lit(_) => false
    case One() | Star(_) => true
    case Plus(r1, r2) => r1.nullable || r2.nullable
    case Comp(r1, r2) => r1.nullable && r2.nullable

  def alphabet(using set: SymbolSet[A]): A = this match
    case Zero() | One() => set.empty
    case Lit(d) => d
    case Plus(r1, r2) => r1.alphabet | r2.alphabet
    case Comp(r1, r2) => r1.alphabet | r2.alphabet
    case Star(r1) => r1.alphabet

  def first(using set: SymbolSet[A]): A = this match
    case Zero() | One() => set.empty
    case Lit(d) => d
    case Plus(r1, r2) => r1.first | r2.first
    case Comp(r1, r2) => if r1.nullable then r1.first | r2.first else r1.first
    case Star(r1) => r1.first

  def reverse: RegEx[A] = this match
    case Zero() | One() | Lit(_) => this
    case Plus(r1, r2) => r1.reverse + r2.reverse
    case Comp(r1, r2) => r2.reverse * r1.reverse
    case Star(r1) => r1.reverse.star

  def deriv(p: A => Boolean): RegEx[A] = this match
    case Zero() | One() => Zero()
    case Lit(d) => if p(d) then One() else Zero()
    case Plus(r1, r2) => r1.deriv(p) + r2.deriv(p)
    case Comp(r1, r2) => if r1.nullable then r1.deriv(p) * r2 + r2.deriv(p) else r1.deriv(p) * r2
    case Star(r1) => r1.deriv(p) * this

  def deriv(using set: SymbolSet[A])(x: set.Symbol): RegEx[A] = deriv(_.contains(x))

  def deriv(using set: SymbolSet[A])(s: List[set.Symbol]): RegEx[A] = s match
    case Nil => this
    case x :: xs =>
      val r1 = deriv(x)
      if r1.isEmpty then Zero() else r1.deriv(xs)

  def contains(using set: SymbolSet[A])(s: List[set.Symbol]): Boolean = deriv(s).nullable

object RegEx:
  def symbol[A](using set: SymbolSet[A])(x: set.Symbol): RegEx[A] = Lit(set.singleton(x))

  def word[A](using set: SymbolSet[A])(xs: List[set.Symbol]): RegEx[A] =
    if xs.isEmpty then One() else xs.map(symbol(_)).reduce(_ * _)

  def symbolSet[A](using SymbolSet[A])(a: A): RegEx[A] =
    if a.isEmpty then Zero() else Lit(a)

  def sum[A](rs: List[RegEx[A]]): RegEx[A] =
    if rs.isEmpty then Zero() else rs.reduce(_ + _)

  def product[A](rs: List[RegEx[A]]): RegEx[A] =
    if rs.isEmpty then One() else rs.reduce(_ * _)

given [A](using set: SymbolSet[A]): SymbolSet[RegEx[A]] with
  import RegEx.*

  def empty: RegEx[A] = Zero()

  type Symbol = List[set.Symbol]

  def singleton(x: Symbol): RegEx[A] = word(x)

  extension (r1: RegEx[A])
    def isEmpty: Boolean = r1.isEmpty
    def contains(x: Symbol): Boolean = r1.contains(x)
    def |(r2: RegEx[A]): RegEx[A] = r1 + r2
    def -(x: Symbol): RegEx[A] = throw UnsupportedOperationException("RegEx subtraction is not supported")

type StrRE = RegEx[CharSet]
