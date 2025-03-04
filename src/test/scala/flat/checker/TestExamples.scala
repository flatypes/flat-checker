//package flat.checker
//
//import org.scalatest.funsuite.AnyFunSuite
//
//class TestExamples extends AnyFunSuite:
//  val exampleDir: os.Path = os.pwd / "examples"
//
//  def mkTest(path: os.Path): Unit =
//    test(path.baseName):
//      Driver.check(path) match
//        case Left(msg) => fail(msg)
//        case Right(value) =>
//
//  os.list(exampleDir)
//    .filter(_.ext == "flat")
//    .foreach(mkTest)
