package scala.meta.internal
package scalahost
package mirrors

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.Global
import scala.util.Properties
import scala.{meta => m}
import scala.meta.internal.semantic.mirrors.CommonMirror
import scala.meta.internal.scalahost.databases.DatabaseOps

class OnlineMirror(val global: Global) extends CommonMirror with DatabaseOps {
  override def toString: String = {
    val compiler = s"the Scala compiler ${Properties.versionString}"
    val settings = global.settings.toConciseString
    s"online mirror for $compiler running with $settings"
  }

  private def units: Seq[g.CompilationUnit] = {
    g.currentRun.units.filter(!_.source.file.name.endsWith(".java")).toList
  }

  def sources: Seq[m.Source] = {
    units.toList.map(_.toSource)
  }

  private var cachedDatabaseKey = (g.currentRun, -1)
  private def recomputeCachedDatabaseKey() = (g.currentRun, g.currentRun.size)
  private var cachedDatabase: m.Database = null
  def database: m.Database = {
    // NOTE: We rely on the fact that compilation units change monotonously,
    // i.e. that we can only add new compilation units, but not remove them.
    if (cachedDatabaseKey != recomputeCachedDatabaseKey()) {
      cachedDatabaseKey = recomputeCachedDatabaseKey()
      cachedDatabase = m.Database(units.map(_.toAttributedSource))
    }
    cachedDatabase
  }
}
