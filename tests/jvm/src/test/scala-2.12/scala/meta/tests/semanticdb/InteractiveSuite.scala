package scala.meta.tests
package semanticdb

import org.scalameta.logger
import munit.FunSuite
import scala.meta.interactive.InteractiveSemanticdb._
import scala.meta.internal.semanticdb.Print
import scala.tools.nsc.interactive.Global

class InteractiveSuite extends FunSuite {
  val compiler: Global = newCompiler(scalacOptions = "-Ywarn-unused:imports" :: Nil)
  def check(
      original: String,
      expected: String
  ): Unit = {
    test(logger.revealWhitespace(original)) {
      val options = List("-P:semanticdb:synthetics:on", "-P:semanticdb:text:on")
      val document = toTextDocument(compiler, original, options)
      val format = scala.meta.metap.Format.Detailed
      val syntax = Print.document(format, document)
      assertNoDiff(syntax, expected)
    }
  }

  check(
    """
import cats.Monad
import cats.implicits._

object ScalafixCatsSimpleImports {
  val a: String = "a"
  val b: BigInt = BigInt(200)
  show"$a $b"

  def foo[M[_]: Monad]: M[String] =
    for {
      r <- "en".pure[M]
      _ <- "to".pure[M]
    } yield r
}
""".stripMargin,
    // Note that scala don't resolve to a symbol, this is a sign that the
    // typer hijacking is not working as expected with interactive.Global.
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 9 entries
       |Occurrences => 25 entries
       |Synthetics => 8 entries
       |
       |Symbols:
       |_empty_/ScalafixCatsSimpleImports. => final object ScalafixCatsSimpleImports extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ScalafixCatsSimpleImports.a. => val method a: String
       |  String => scala/Predef.String#
       |_empty_/ScalafixCatsSimpleImports.b. => val method b: BigInt
       |  BigInt => scala/package.BigInt#
       |_empty_/ScalafixCatsSimpleImports.foo(). => method foo[M[_]](implicit evidence$1: Monad[M]): M[String]
       |  M => _empty_/ScalafixCatsSimpleImports.foo().[M]
       |  _ => _empty_/ScalafixCatsSimpleImports.foo().[M][_]
       |  evidence$1 => _empty_/ScalafixCatsSimpleImports.foo().(evidence$1)
       |  Monad => cats/Monad#
       |  String => scala/Predef.String#
       |_empty_/ScalafixCatsSimpleImports.foo().(evidence$1) => implicit param evidence$1: Monad[M]
       |  Monad => cats/Monad#
       |  M => _empty_/ScalafixCatsSimpleImports.foo().[M]
       |_empty_/ScalafixCatsSimpleImports.foo().[M] => typeparam M[_]
       |  _ => _empty_/ScalafixCatsSimpleImports.foo().[M][_]
       |_empty_/ScalafixCatsSimpleImports.foo().[M][_] => typeparam _
       |local0 => param r: String
       |  String => java/lang/String#
       |local1 => param _: String
       |  String => java/lang/String#
       |
       |Occurrences:
       |[1:7..1:11): cats => cats/
       |[1:12..1:17): Monad => cats/Monad#
       |[1:12..1:17): Monad => cats/Monad.
       |[2:7..2:11): cats => cats/
       |[2:12..2:21): implicits => cats/implicits.
       |[4:7..4:32): ScalafixCatsSimpleImports <= _empty_/ScalafixCatsSimpleImports.
       |[5:6..5:7): a <= _empty_/ScalafixCatsSimpleImports.a.
       |[5:9..5:15): String => scala/Predef.String#
       |[6:6..6:7): b <= _empty_/ScalafixCatsSimpleImports.b.
       |[6:9..6:15): BigInt => scala/package.BigInt#
       |[6:18..6:24): BigInt => scala/package.BigInt.
       |[7:2..7:6): show => cats/Show.ShowInterpolator#show().
       |[7:8..7:9): a => cats/Show.Shown.mat().
       |[7:11..7:12): b => cats/Show.Shown.mat().
       |[9:6..9:9): foo <= _empty_/ScalafixCatsSimpleImports.foo().
       |[9:10..9:11): M <= _empty_/ScalafixCatsSimpleImports.foo().[M]
       |[9:16..9:21): Monad => cats/Monad#
       |[9:24..9:25): M => _empty_/ScalafixCatsSimpleImports.foo().[M]
       |[9:26..9:32): String => scala/Predef.String#
       |[11:6..11:7): r <= local0
       |[11:16..11:20): pure => cats/syntax/ApplicativeIdOps#pure().
       |[11:21..11:22): M => _empty_/ScalafixCatsSimpleImports.foo().[M]
       |[12:16..12:20): pure => cats/syntax/ApplicativeIdOps#pure().
       |[12:21..12:22): M => _empty_/ScalafixCatsSimpleImports.foo().[M]
       |[13:12..13:13): r => local0
       |
       |Synthetics:
       |[6:18..6:24): BigInt => *.apply
       |  apply => scala/math/BigInt.apply().
       |[7:2..7:2):  => *.apply
       |  apply => scala/StringContext.apply().
       |[7:2..7:13): show"$a $b" => showInterpolator(*)
       |  showInterpolator => cats/syntax/ShowSyntax#showInterpolator().
       |[7:8..7:9): a => Shown.mat[String](*)(catsStdShowForString)
       |  Shown => cats/Show.Shown.
       |  mat => cats/Show.Shown.mat().
       |  String => scala/Predef.String#
       |  catsStdShowForString => cats/instances/StringInstances#catsStdShowForString.
       |[7:11..7:12): b => Shown.mat[BigInt](*)(catsStdShowForBigInt)
       |  Shown => cats/Show.Shown.
       |  mat => cats/Show.Shown.mat().
       |  BigInt => scala/package.BigInt#
       |  catsStdShowForBigInt => cats/instances/BigIntInstances#catsStdShowForBigInt.
       |[10:4..13:13): for {
       |      r <- "en".pure[M]
       |      _ <- "to".pure[M]
       |    } yield r => implicits.toFlatMapOps[M, String](orig("en".pure[M])(evidence$1))(evidence$1).flatMap[String]({(r) => implicits.toFunctorOps[M, String](orig("to".pure[M])(evidence$1))(evidence$1).map[String]({(_) => orig(r)})})
       |  implicits => cats/implicits.
       |  toFlatMapOps => cats/FlatMap.ToFlatMapOps#toFlatMapOps().
       |  M => _empty_/ScalafixCatsSimpleImports.foo().[M]
       |  String => java/lang/String#
       |  evidence$1 => _empty_/ScalafixCatsSimpleImports.foo().(evidence$1)
       |  flatMap => cats/FlatMap.Ops#flatMap().
       |  r => local0
       |  toFunctorOps => cats/Functor.ToFunctorOps#toFunctorOps().
       |  map => cats/Functor.Ops#map().
       |  _ => local1
       |[11:11..11:15): "en" => implicits.catsSyntaxApplicativeId[String](*)
       |  implicits => cats/implicits.
       |  catsSyntaxApplicativeId => cats/syntax/ApplicativeSyntax#catsSyntaxApplicativeId().
       |  String => java/lang/String#
       |[12:11..12:15): "to" => implicits.catsSyntaxApplicativeId[String](*)
       |  implicits => cats/implicits.
       |  catsSyntaxApplicativeId => cats/syntax/ApplicativeSyntax#catsSyntaxApplicativeId().
       |  String => java/lang/String#""".stripMargin
  )

  // This tests a case where SymbolOps.toSemantic crashes
  check(
    """
      |object b {
      |  def add(a: In) = 1
      |}""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 3 entries
       |Occurrences => 4 entries
       |Diagnostics => 1 entries
       |
       |Symbols:
       |_empty_/b. => final object b extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/b.add(). => method add(a): Int
       |  a => _empty_/b.add().(a)
       |  Int => scala/Int#
       |_empty_/b.add().(a) => param a
       |
       |Occurrences:
       |[1:7..1:8): b <= _empty_/b.
       |[2:6..2:9): add <= _empty_/b.add().
       |[2:10..2:11): a <= _empty_/b.add().(a)
       |[2:13..2:15): In => local0
       |
       |Diagnostics:
       |[2:13..2:15) [error] not found: type In
    """.stripMargin
  )
}
