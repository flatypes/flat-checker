package flat.checker.domain

/** A generic regular expression.
 * Names mirror the definition of a Kleene algebra. */
enum RegEx[T, D]:
  case REZero()
  case REOne()
  case RELit(dom: D)
  case REPlus(left: RegEx[T, D], right: RegEx[T, D])
  case REComp(left: RegEx[T, D], right: RegEx[T, D])
  case REStar(inner: RegEx[T, D])

  def +(that: RegEx[T, D]): RegEx[T, D] = (this, that) match
    case (REZero(), r) => r // 0 + r = r
    case (r, REZero()) => r // r + 0 = r
    case (r, REOne()) if r.nullable => r // (r + 1) + 1 = r
    case (REOne(), r) if r.nullable => r // 1 + (r + 1) = r
    case (REPlus(r1, r2), r) if r == r1 || r == r2 => this // (r1 + r2) + r1 = (r1 + r2)
    case (r1, r2) if r1 == r2 => r1 // r + r = r
    case (r1, r2) => REPlus(r1, r2)

  def *(that: RegEx[T, D]): RegEx[T, D] = (this, that) match
    case (REZero(), _) | (_, REZero()) => REZero() // 0 * r = r * 0 = 0
    case (REOne(), r) => r // 1 * r = r
    case (r, REOne()) => r // r * 1 = r
    case (r1, r2) => REComp(r1, r2)

  def star: RegEx[T, D] = this match
    case REZero() | REOne() => REOne() // 0.star = 1.star = 1
    case r => REStar(r)

  def plus: RegEx[T, D] = this * this.star

  def opt: RegEx[T, D] = REOne() + this

  def ^(n: Int): RegEx[T, D] = n match
    case _ if n < 0 => throw IllegalArgumentException("negative exponent")
    case 0 => REOne()
    case 1 => this
    case _ => this * (this ^ (n - 1))

  def loop(min: Int, max: Option[Int]): RegEx[T, D] =
    require(min >= 0)
    max match
      case Some(m) if m < min => throw IllegalArgumentException("max < min")
      case Some(m) => (this ^ min) * RegEx.sum((for k <- 0 to (m - min) yield this ^ k).toList)
      case None => (this ^ min) * this.star

  def isEmpty(using Lattice[D]): Boolean = this match
    case REZero() => true
    case REOne() | REStar(_) => false
    case RELit(d) => d.isEmpty
    case REPlus(r1, r2) => r1.isEmpty && r2.isEmpty
    case REComp(r1, r2) => r1.isEmpty || r2.isEmpty

  def nonEmpty(using Lattice[D]): Boolean = !isEmpty

  def nullable: Boolean = this match
    case REZero() | RELit(_) => false
    case REOne() | REStar(_) => true
    case REPlus(r1, r2) => r1.nullable || r2.nullable
    case REComp(r1, r2) => r1.nullable && r2.nullable

  def alphabet(using lattice: Lattice[D]): D = this match
    case REZero() | REOne() => lattice.bot
    case RELit(d) => d
    case REPlus(r1, r2) => r1.alphabet | r2.alphabet
    case REComp(r1, r2) => r1.alphabet | r2.alphabet
    case REStar(r1) => r1.alphabet

  def first(using lattice: Lattice[D]): D = this match
    case REZero() | REOne() => lattice.bot
    case RELit(d) => d
    case REPlus(r1, r2) => r1.first | r2.first
    case REComp(r1, r2) => if r1.nullable then r1.first | r2.first else r1.first
    case REStar(r1) => r1.first

  def reverse: RegEx[T, D] = this match
    case REZero() | REOne() => this
    case RELit(d) => this
    case REPlus(r1, r2) => r1.reverse + r2.reverse
    case REComp(r1, r2) => r2.reverse * r1.reverse
    case REStar(r1) => r1.reverse.star

  def deriv(p: D => Boolean): RegEx[T, D] = this match
    case REZero() | REOne() => REZero()
    case RELit(d) => if p(d) then REOne() else REZero()
    case REPlus(r1, r2) => r1.deriv(p) + r2.deriv(p)
    case REComp(r1, r2) => r1.deriv(p) * r2 + (if r1.nullable then r2.deriv(p) else REZero())
    case REStar(r1) => r1.deriv(p) * this

  def deriv(x: T)(using Domain[T, D]): RegEx[T, D] = deriv(_.contains(x))

  def deriv(s: List[T])(using Domain[T, D]): RegEx[T, D] = s match
    case Nil => this
    case x :: xs =>
      val r1 = deriv(x)
      if r1.isEmpty then REZero() else r1.deriv(xs)

  def contains(s: List[T])(using Domain[T, D]): Boolean = deriv(s) == REOne()

object RegEx:
  def empty[T, D]: RegEx[T, D] = REZero()

  def apply[T, D](x: T)(using domain: Domain[T, D]): RegEx[T, D] = RELit(domain.mkSingleton(x))

  def apply[T, D](xs: List[T])(using Domain[T, D]): RegEx[T, D] =
    if xs.isEmpty then REOne() else xs.map(apply(_)).reduce(_ * _)

  def lit[T, D](d: D)(using Domain[T, D]): RegEx[T, D] =
    if d.isEmpty then REZero() else RELit(d)

  def sum[T, D](rs: List[RegEx[T, D]]): RegEx[T, D] =
    if rs.isEmpty then REZero() else rs.reduce(_ + _)

  def product[T, D](rs: List[RegEx[T, D]]): RegEx[T, D] =
    if rs.isEmpty then REOne() else rs.reduce(_ * _)

given [T, D](using domain: Domain[T, D]): Domain[List[T], RegEx[T, D]] with
  import RegEx.*

  def top: RegEx[T, D] = REStar(RELit(domain.top))

  def bot: RegEx[T, D] = REZero()

  def mkSingleton(xs: List[T]): RegEx[T, D] = RegEx(xs)

  extension (r1: RegEx[T, D])
    def isEmpty: Boolean = r1.isEmpty

    def subsetOf(r2: RegEx[T, D]): Boolean = RESub.check(r1, r2)

    def |(r2: RegEx[T, D]): RegEx[T, D] = r1 + r2

    def &(r2: RegEx[T, D]): RegEx[T, D] = throw UnsupportedOperationException("intersection not implemented")

    def unary_~ : RegEx[T, D] = throw UnsupportedOperationException("complement not implemented")

    def contains(xs: List[T]): Boolean = r1.contains(xs)

    def representative: List[T] = r1 match
      case REZero() => throw IllegalArgumentException("empty regex has no representative")
      case REOne() => Nil
      case RELit(d) => List(d.representative)
      case REPlus(r1, _) => r1.representative
      case REComp(r1, r2) => r1.representative ++ r2.representative
      case REStar(r1) => r1.representative

    def -(xs: List[T]): RegEx[T, D] = throw UnsupportedOperationException("remove not implemented")

type CharRE = RegEx[Char, CharSet]
