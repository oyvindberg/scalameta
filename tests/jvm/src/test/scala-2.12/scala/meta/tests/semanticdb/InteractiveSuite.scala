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
//      println(syntax)
      assertNoDiff(syntax, expected)
    }
  }

  check(
    """
import scala.meta.tests.semanticdb.Sphynx._

object Bug {

  def fun[F[_]: Monad, A](a1: A, a2: A): F[A] =
    for {
      o1 <- a1.pure[F]
      _ <- a2.pure[F]
    } yield o1

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
       |Symbols => 10 entries
       |Occurrences => 24 entries
       |Synthetics => 3 entries
       |
       |Symbols:
       |_empty_/Bug. => final object Bug extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/Bug.fun(). => method fun[F[_], A](a1: A, a2: A)(implicit evidence$1: Monad[F]): F[A]
       |  F => _empty_/Bug.fun().[F]
       |  _ => _empty_/Bug.fun().[F][_]
       |  A => _empty_/Bug.fun().[A]
       |  a1 => _empty_/Bug.fun().(a1)
       |  a2 => _empty_/Bug.fun().(a2)
       |  evidence$1 => _empty_/Bug.fun().(evidence$1)
       |  Monad => scala/meta/tests/semanticdb/Sphynx.Monad#
       |_empty_/Bug.fun().(a1) => param a1: A
       |  A => _empty_/Bug.fun().[A]
       |_empty_/Bug.fun().(a2) => param a2: A
       |  A => _empty_/Bug.fun().[A]
       |_empty_/Bug.fun().(evidence$1) => implicit param evidence$1: Monad[F]
       |  Monad => scala/meta/tests/semanticdb/Sphynx.Monad#
       |  F => _empty_/Bug.fun().[F]
       |_empty_/Bug.fun().[A] => typeparam A
       |_empty_/Bug.fun().[F] => typeparam F[_]
       |  _ => _empty_/Bug.fun().[F][_]
       |_empty_/Bug.fun().[F][_] => typeparam _
       |local0 => param o1: A
       |  A => _empty_/Bug.fun().[A]
       |local1 => param _: A
       |  A => _empty_/Bug.fun().[A]
       |
       |Occurrences:
       |[1:7..1:12): scala => scala/
       |[1:13..1:17): meta => scala/meta/
       |[1:18..1:23): tests => scala/meta/tests/
       |[1:24..1:34): semanticdb => scala/meta/tests/semanticdb/
       |[1:35..1:41): Sphynx => scala/meta/tests/semanticdb/Sphynx.
       |[3:7..3:10): Bug <= _empty_/Bug.
       |[5:6..5:9): fun <= _empty_/Bug.fun().
       |[5:10..5:11): F <= _empty_/Bug.fun().[F]
       |[5:16..5:21): Monad => scala/meta/tests/semanticdb/Sphynx.Monad#
       |[5:23..5:24): A <= _empty_/Bug.fun().[A]
       |[5:26..5:28): a1 <= _empty_/Bug.fun().(a1)
       |[5:30..5:31): A => _empty_/Bug.fun().[A]
       |[5:33..5:35): a2 <= _empty_/Bug.fun().(a2)
       |[5:37..5:38): A => _empty_/Bug.fun().[A]
       |[5:41..5:42): F => _empty_/Bug.fun().[F]
       |[5:43..5:44): A => _empty_/Bug.fun().[A]
       |[7:6..7:8): o1 <= local0
       |[7:12..7:14): a1 => _empty_/Bug.fun().(a1)
       |[7:15..7:19): pure => scala/meta/tests/semanticdb/Sphynx.ApplicativeIdOps#pure().
       |[7:20..7:21): F => _empty_/Bug.fun().[F]
       |[8:11..8:13): a2 => _empty_/Bug.fun().(a2)
       |[8:14..8:18): pure => scala/meta/tests/semanticdb/Sphynx.ApplicativeIdOps#pure().
       |[8:19..8:20): F => _empty_/Bug.fun().[F]
       |[9:12..9:14): o1 => local0
       |
       |Synthetics:
       |[6:4..9:14): for {
       |      o1 <- a1.pure[F]
       |      _ <- a2.pure[F]
       |    } yield o1 => Sphynx.ToFlatMapOps[F, A](orig(a1.pure[F])(evidence$1))(evidence$1).flatMap[A]({(o1) => Sphynx.ToFunctorOps[F, A](orig(a2.pure[F])(evidence$1))(evidence$1).map[A]({(_) => orig(o1)})})
       |  Sphynx => scala/meta/tests/semanticdb/Sphynx.
       |  ToFlatMapOps => scala/meta/tests/semanticdb/Sphynx.ToFlatMapOps().
       |  F => _empty_/Bug.fun().[F]
       |  A => _empty_/Bug.fun().[A]
       |  evidence$1 => _empty_/Bug.fun().(evidence$1)
       |  flatMap => scala/meta/tests/semanticdb/Sphynx.ToFlatMapOps#flatMap().
       |  o1 => local0
       |  ToFunctorOps => scala/meta/tests/semanticdb/Sphynx.ToFunctorOps().
       |  map => scala/meta/tests/semanticdb/Sphynx.ToFunctorOps#map().
       |  _ => local1
       |[7:12..7:14): a1 => Sphynx.ApplicativeIdOps[A](*)
       |  Sphynx => scala/meta/tests/semanticdb/Sphynx.
       |  ApplicativeIdOps => scala/meta/tests/semanticdb/Sphynx.ApplicativeIdOps().
       |  A => _empty_/Bug.fun().[A]
       |[8:11..8:13): a2 => Sphynx.ApplicativeIdOps[A](*)
       |  Sphynx => scala/meta/tests/semanticdb/Sphynx.
       |  ApplicativeIdOps => scala/meta/tests/semanticdb/Sphynx.ApplicativeIdOps().
       |  A => _empty_/Bug.fun().[A]""".stripMargin
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
