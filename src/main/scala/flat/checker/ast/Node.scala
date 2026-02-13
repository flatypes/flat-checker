package flat.checker.ast

trait Node extends Product:
  private lazy val subnodes: List[Node] =
    productIterator.toList.flatMap:
      case node: Node => List(node)
      case nodes@(_: Node) :: _ => nodes.asInstanceOf[List[Node]]
      case _ => Nil

  def traverse(pf: PartialFunction[Node, Unit]): Unit =
    if pf.isDefinedAt(this) then
      pf(this)

    subnodes.foreach(_.traverse(pf))

  def collect[T](pf: PartialFunction[Node, T]): List[T] =
    if pf.isDefinedAt(this) then List(pf(this)) else subnodes.flatMap(_.collect(pf))

  def collectFirst[T](pf: PartialFunction[Node, T]): Option[T] =
    if pf.isDefinedAt(this) then Some(pf(this)) else subnodes.collectFirst(Function.unlift(_.collectFirst(pf)))

  def transform[N <: Node](pf: PartialFunction[Node, N]): N =
    if pf.isDefinedAt(this) then
      pf(this)
    else
      val args = productIterator.toList.map:
        case node: Node => node.transform(pf)
        case nodes@(_: Node) :: _ => nodes.asInstanceOf[List[Node]].map(_.transform(pf))
        case const => const
      if args == productIterator.toList then
        this.asInstanceOf[N]
      else
        val primaryConstructor = getClass.getConstructors.head
        primaryConstructor.newInstance(args *).asInstanceOf[N]
