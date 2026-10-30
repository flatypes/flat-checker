package flat.checker.domain

trait LitOps[T, S]:
  def empty: S

  def full: S

  def singleton(x: T): S

  def union(as: List[S]): S = as.foldLeft(empty)(_ | _)

  extension (a: S)
    def isEmpty: Boolean
    def nonEmpty: Boolean = !a.isEmpty
    def contains(x: T): Boolean
    def |(b: S): S
    def -(x: T): S
//def &(xs: List[T]): S = xs.filter(a.contains).foldLeft(empty) { (b, x) => b | singleton(x) }
//def --(xs: List[T]): S = xs.filter(a.contains).foldLeft(a) { (b, x) => b - x }


/** Generic regular expression. Names mirror the definition of a Kleene algebra. */
enum RegExp[T, S]:
  case Zero()
  case One()
  case Lit(set: S)
  case Plus(left: RegExp[T, S], right: RegExp[T, S])
  case Comp(left: RegExp[T, S], right: RegExp[T, S])
  case Star(part: RegExp[T, S])

  /** Abbreviated name for pretty printing. */
  var name = ""

  def isEmpty(using LitOps[T, S]): Boolean = this match
    case Zero() => true
    case One() | Star(_) => false
    case Lit(a) => a.isEmpty
    case Plus(r1, r2) => r1.isEmpty && r2.isEmpty
    case Comp(r1, r2) => r1.isEmpty || r2.isEmpty

  def nonEmpty(using LitOps[T, S]): Boolean = !isEmpty

  def nullable: Boolean = this match
    case Zero() | Lit(_) => false
    case One() | Star(_) => true
    case Plus(r1, r2) => r1.nullable || r2.nullable
    case Comp(r1, r2) => r1.nullable && r2.nullable

  def reverse: RegExp[T, S] = this match
    case Zero() => Zero()
    case One() => One()
    case Lit(a) => Lit(a)
    case Plus(r1, r2) => Plus(r1.reverse, r2.reverse)
    case Comp(r1, r2) => Comp(r2.reverse, r1.reverse)
    case Star(r1) => Star(r1.reverse)

  def first(using litOps: LitOps[T, S]): S = this match
    case Zero() | One() => litOps.empty
    case Lit(a) => a
    case Plus(r1, r2) => r1.first | r2.first
    case Comp(r1, r2) => if r1.nullable then r1.first | r2.first else r1.first
    case Star(r1) => r1.first

  // Simplification by construction
  def +(that: RegExp[T, S]): RegExp[T, S] = (this, that) match
    case (Zero(), r) => r // 0 + r = r
    case (r, Zero()) => r // r + 0 = r
    case (r, One()) if r.nullable => r // (r + 1) + 1 = r
    case (One(), r) if r.nullable => r // 1 + (r + 1) = r
    case (Plus(r1, r2), r) if r == r1 || r == r2 => this // (r1 + r2) + r1 = (r1 + r2)
    case (r1, r2) if r1 == r2 => r1 // r + r = r
    case (r1, r2) => Plus(r1, r2)

  def *(that: RegExp[T, S]): RegExp[T, S] = (this, that) match
    case (Zero(), _) | (_, Zero()) => Zero() // 0 * r = r * 0 = 0
    case (One(), r) => r // 1 * r = r
    case (r, One()) => r // r * 1 = r
    case (r1, r2) => Comp(r1, r2)

  def star: RegExp[T, S] = this match
    case Zero() | One() => One() // 0.star = 1.star = 1
    case r => Star(r)

  def plus: RegExp[T, S] = this * this.star

  def opt: RegExp[T, S] = One() + this

  def ^(n: Int): RegExp[T, S] = n match
    case _ if n < 0 => throw IllegalArgumentException("negative exponent")
    case 0 => One()
    case 1 => this
    case _ => this * (this ^ (n - 1))

  def loop(min: Int, max: Option[Int]): RegExp[T, S] =
    require(min >= 0)
    max match
      case Some(m) if m < min => throw IllegalArgumentException("max < min")
      case Some(m) => (this ^ min) * RegExp.sum((for k <- 0 to (m - min) yield this ^ k).toList)
      case None => (this ^ min) * this.star

object RegExp:
  def full[T, S](using litOps: LitOps[T, S]): RegExp[T, S] = Lit(litOps.full).star

  def fromChar[T, S](x: T)(using litOps: LitOps[T, S]): RegExp[T, S] = Lit(litOps.singleton(x))

  def fromString[T, S](xs: List[T])(using litOps: LitOps[T, S]): RegExp[T, S] =
    if xs.isEmpty then One() else xs.map(fromChar).reduceLeft(_ * _)

  def fromCharSet[T, S](set: S)(using LitOps[T, S]): RegExp[T, S] = if set.isEmpty then Zero() else Lit(set)

  def sum[T, S](rs: List[RegExp[T, S]]): RegExp[T, S] = rs match
    case Nil => Zero()
    case r :: Nil => r
    case r :: rs => r + sum(rs)

  def product[T, S](rs: List[RegExp[T, S]]): RegExp[T, S] = rs match
    case Nil => One()
    case r :: Nil => r
    case r :: rs => r * product(rs)

given [T, S](using litOps: LitOps[T, S]): LitOps[List[T], RegExp[T, S]] with
  def empty = RegExp.Zero()

  def full: RegExp[T, S] = RegExp.full

  def singleton(x: List[T]): RegExp[T, S] = RegExp.fromString(x)

  import flat.checker.domain.REAPI.{deriv, filterNe}

  extension (r: RegExp[T, S])
    def isEmpty: Boolean = r.isEmpty
    def contains(s: List[T]): Boolean = r.deriv(s).nullable
    def |(r2: RegExp[T, S]): RegExp[T, S] = r + r2
    def -(s: List[T]): RegExp[T, S] = r.filterNe(s)

type CharRE = RegExp[Char, CharSet]
type StrListRE = RegExp[List[Char], CharRE]