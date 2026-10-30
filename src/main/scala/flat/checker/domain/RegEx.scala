package flat.checker.domain

/** Generic regular expression. Names mirror the definition of a Kleene algebra. */
enum RegEx[T, S]:
  case Zero()
  case One()
  case Lit(set: S)
  case Plus(left: RegEx[T, S], right: RegEx[T, S])
  case Comp(left: RegEx[T, S], right: RegEx[T, S])
  case Star(part: RegEx[T, S])

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

  def reverse: RegEx[T, S] = this match
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
  def +(that: RegEx[T, S]): RegEx[T, S] = (this, that) match
    case (Zero(), r) => r // 0 + r = r
    case (r, Zero()) => r // r + 0 = r
    case (r, One()) if r.nullable => r // (r + 1) + 1 = r
    case (One(), r) if r.nullable => r // 1 + (r + 1) = r
    case (Plus(r1, r2), r) if r == r1 || r == r2 => this // (r1 + r2) + r1 = (r1 + r2)
    case (r1, r2) if r1 == r2 => r1 // r + r = r
    case (r1, r2) => Plus(r1, r2)

  def *(that: RegEx[T, S]): RegEx[T, S] = (this, that) match
    case (Zero(), _) | (_, Zero()) => Zero() // 0 * r = r * 0 = 0
    case (One(), r) => r // 1 * r = r
    case (r, One()) => r // r * 1 = r
    case (r1, r2) => Comp(r1, r2)

  def star: RegEx[T, S] = this match
    case Zero() | One() => One() // 0.star = 1.star = 1
    case r => Star(r)

  def plus: RegEx[T, S] = this * this.star

  def opt: RegEx[T, S] = One() + this

  def ^(n: Int): RegEx[T, S] = n match
    case _ if n < 0 => throw IllegalArgumentException("negative exponent")
    case 0 => One()
    case 1 => this
    case _ => this * (this ^ (n - 1))

  def loop(min: Int, max: Option[Int]): RegEx[T, S] =
    require(min >= 0)
    max match
      case Some(m) if m < min => throw IllegalArgumentException("max < min")
      case Some(m) => (this ^ min) * RegEx.sum((for k <- 0 to (m - min) yield this ^ k).toList)
      case None => (this ^ min) * this.star

object RegEx:
  def full[T, S](using litOps: LitOps[T, S]): RegEx[T, S] = Lit(litOps.full).star

  def fromChar[T, S](x: T)(using litOps: LitOps[T, S]): RegEx[T, S] = Lit(litOps.singleton(x))

  def fromString[T, S](xs: List[T])(using litOps: LitOps[T, S]): RegEx[T, S] =
    if xs.isEmpty then One() else xs.map(fromChar).reduceLeft(_ * _)

  def fromCharSet[T, S](set: S)(using LitOps[T, S]): RegEx[T, S] = if set.isEmpty then Zero() else Lit(set)

  def sum[T, S](rs: List[RegEx[T, S]]): RegEx[T, S] = rs match
    case Nil => Zero()
    case r :: Nil => r
    case r :: rs => r + sum(rs)

  def product[T, S](rs: List[RegEx[T, S]]): RegEx[T, S] = rs match
    case Nil => One()
    case r :: Nil => r
    case r :: rs => r * product(rs)

given [T, S](using litOps: LitOps[T, S]): LitOps[List[T], RegEx[T, S]] with
  def empty = RegEx.Zero()

  def full: RegEx[T, S] = RegEx.full

  def singleton(x: List[T]): RegEx[T, S] = RegEx.fromString(x)

  import flat.checker.domain.REOps.{deriv, filterNe}

  extension (r: RegEx[T, S])
    def isEmpty: Boolean = r.isEmpty
    def contains(s: List[T]): Boolean = r.deriv(s).nullable
    def |(r2: RegEx[T, S]): RegEx[T, S] = r + r2
    def -(s: List[T]): RegEx[T, S] = r.filterNe(s)

type CharRE = RegEx[Char, CharSet]
type StrRE = RegEx[List[Char], CharRE]