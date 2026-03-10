package flat.checker.verifier

import flat.checker.ast.*
import org.eclipse.lsp4j.*

final case class FunSig(params: List[Decl], returnParams: List[Decl], requires: List[Expr], ensures: List[Expr])

final case class State(signatures: Map[String, FunSig], // function name -> signature
                       currentFun: String, // function under execution
                       localCtx: Map[String, Sort], // program variable -> sort

                       symbolicCtx: Map[String, Sort], // symbolic variable -> sort
                       symbolicValues: Map[String, Expr], // program variable -> symbolic value
                       freshCounts: Map[String, Int], // name -> number of fresh variables

                       constraints: List[Expr] = List.empty, // logical constraints
                       constraintAnnots: List[String] = List.empty)
