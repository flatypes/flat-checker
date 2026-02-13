package flat.regex

trait Domain

case object TopDomain extends Domain

final case class ProductDomain(elemDomains: List[Domain]) extends Domain

final case class SeqDomain(elemDomain: Domain) extends Domain

final case class SetDomain(elemDomain: Domain) extends Domain