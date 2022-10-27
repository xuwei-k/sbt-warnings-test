package sbtwarnings

import sbt.internal.inc.Analysis
import xsbti.Position
import xsbti.Severity
import sbt._
import sbt.Keys._
import sbt.plugins.JvmPlugin
import sjsonnew.Builder
import sjsonnew.JsonFormat
import sjsonnew.BasicJsonProtocol
import sjsonnew.BasicJsonProtocol._
import sjsonnew.Unbuilder
import sjsonnew.support.scalajson.unsafe.PrettyPrinter

object SbtWarningsPlugin extends AutoPlugin {
  object autoImport {
    val warningsDiffFile = settingKey[File]("")
    val warningsCurrentFile = settingKey[File]("")
    val warningsPreviousFile = settingKey[File]("")
    val warnings = taskKey[Warnings]("")
    val warningsDiff = taskKey[Option[WarningDiff]]("")
    val warningsAll = taskKey[Warnings]("")
    val warningsPrevious = taskKey[Warnings]("")
  }

  case class Pos(
    line: Int,
    content: String,
    path: String
  )

  object Pos {
    def fromXsbt(p: Position): Pos = Pos(
      line = p.line().get(),
      content = p.lineContent(),
      path = p.sourcePath().get()
    )
  }

  case class Warning(message: String, position: Pos) {
    override def toString = this.toJsonString
  }

  object Warning {
    implicit val instance: JsonFormat[Warning] = {
      implicit val position: JsonFormat[Pos] = BasicJsonProtocol.caseClass3(Pos.apply, Pos.unapply)(
        "line",
        "content",
        "path"
      )
      caseClass2(Warning.apply, Warning.unapply)(
        "message",
        "position"
      )
    }
  }

  case class WarningDiff(added: Seq[Warning], removed: Seq[Warning]) {
    override def toString = this.toJsonString
  }

  object WarningDiff {
    implicit val instance: JsonFormat[WarningDiff] =
      caseClass2(WarningDiff.apply, WarningDiff.unapply)(
        "added",
        "removed"
      )
  }

  type Warnings = Seq[Warning]
  def loadWarningsFromJsonFile(file: File): Warnings = {
    val unbuilder = new Unbuilder(sjsonnew.support.scalajson.unsafe.Converter.facade)
    val json = sjsonnew.support.scalajson.unsafe.Parser.parseFromFile(file).get
    implicitly[JsonFormat[Warnings]].read(Option(json), unbuilder)
  }

  import autoImport._

  private implicit class JsonClassOps[A](private val self: A) extends AnyVal {
    def toJsonString(implicit format: JsonFormat[A]): String = {
      val builder = new Builder(sjsonnew.support.scalajson.unsafe.Converter.facade)
      format.write(self, builder)
      PrettyPrinter.apply(
        builder.result.getOrElse(sys.error("invalid json"))
      )
    }
  }

  override def trigger = allRequirements

  override def requires: Plugins = JvmPlugin

  private[this] val warningConfigs = Seq(Compile, Test)

  override def projectSettings: Seq[Def.Setting[?]] = warningConfigs.flatMap { x =>
    (x / warnings) := {
      val analysis = (x / Keys.compile).value match {
        case a: Analysis => a
      }
      val problems = analysis.infos.allInfos.values.flatMap(i => i.getReportedProblems ++ i.getUnreportedProblems)
      problems.collect {
        case p if p.severity() == Severity.Warn =>
          Warning(message = p.message(), position = Pos.fromXsbt(p.position()))
      }.toSeq
    }
  }

  private def dir = "warnings"

  override def buildSettings: Seq[Def.Setting[?]] = Def.settings(
    warningsDiffFile := {
      (LocalRootProject / target).value / dir / "warnings.json"
    },
    warningsCurrentFile := {
      (LocalRootProject / target).value / dir / "warnings-diff.json"
    },
    warningsPreviousFile := {
      (LocalRootProject / target).value / dir / "warnings-previous.json"
    },
    warningsPrevious := {
      val f = warningsPreviousFile.value
      val s = streams.value
      if (f.isFile) {
        loadWarningsFromJsonFile(f)
      } else {
        s.log.warn(s"$f does not exists")
        Nil
      }
    },
    warningsDiff := Def.taskDyn {
      warningsPrevious.?.value match {
        case Some(previous) =>
          def f(x: Warnings): Map[String, Warnings] = {
            x.groupBy(_.position.path).map { case (k, v) => k -> v.sortBy(_.position.line) }
          }

          Def.task[Option[WarningDiff]] {
            val current = warningsAll.value
            val previous1 = f(previous)
            val current1 = f(current)
            val added = List.newBuilder[Warning]
            val removed = List.newBuilder[Warning]

            // TODO more better algorithm

            current1.foreach {
              case (path, currentValues) =>
                previous1.get(path) match {
                  case Some(previousValues) =>
                    currentValues.lengthCompare(previousValues.size) match {
                      case 0 =>
                        if (
                          (currentValues, previousValues).zipped.forall {
                            _.message == _.message
                          }
                        ) {
                          // same
                        } else {
                          added ++= currentValues
                        }
                      case n if n > 0 =>
                        added ++= currentValues
                      case n if n < 0 =>
                      // maybe removed
                    }
                  case None =>
                    // all new warnings. if no rename file
                    added ++= currentValues
                }
            }

            previous1.foreach {
              case (path, previousValues) =>
                current1.get(path) match {
                  case Some(currentValues) =>
                    currentValues.lengthCompare(previousValues.size) match {
                      case 0 =>
                      case n if n > 0 =>
                      // maybe added
                      case n if n < 0 =>
                        removed ++= previousValues
                    }
                  case None =>
                    removed ++= previousValues
                }
            }
            val result = WarningDiff(
              added = added.result().distinct,
              removed = removed.result().distinct
            )

            warningsDiffFile.?.value match {
              case Some(diffFile) =>
                streams.value.log.info(s"write to ${diffFile}")
                IO.write(diffFile, result.toJsonString)
              case _ =>
                streams.value.log.warn(s"${warningsDiffFile.key.label} undefined")
            }
            Some(result)
          }
        case None =>
          val s = streams.value
          Def.task[Option[WarningDiff]] {
            s.log.warn(s"empty ${warningsPrevious.key.label}")
            None
          }
      }
    }.value,
    warningsAll := Def.taskDyn {
      val extracted = Project.extract(state.value)
      val currentBuildUri = extracted.currentRef.build
      extracted.structure.units
        .apply(currentBuildUri)
        .defined
        .values
        .filter(
          _.autoPlugins.contains(SbtWarningsPlugin)
        )
        .toList
        .flatMap { p =>
          warningConfigs.map { x =>
            LocalProject(p.id) / x / warnings
          }
        }
        .join
        .map(_.flatten)
    }.value,
    warningsAll := {
      val value = warningsAll.value
      warningsCurrentFile.?.value match {
        case Some(f) =>
          IO.write(f, value.toJsonString)
        case None =>
      }
      value
    }
  )
}
