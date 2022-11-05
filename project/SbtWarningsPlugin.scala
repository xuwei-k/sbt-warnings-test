package sbtwarnings

import sbt._
import sbt.Keys._
import sbt.internal.inc.Analysis
import sbt.plugins.JvmPlugin
import sjsonnew.BasicJsonProtocol
import sjsonnew.BasicJsonProtocol._
import sjsonnew.Builder
import sjsonnew.JsonFormat
import sjsonnew.Unbuilder
import sjsonnew.support.scalajson.unsafe.PrettyPrinter
import xsbti.Position
import xsbti.Problem
import xsbti.Severity

object SbtWarningsPlugin extends AutoPlugin {
  object autoImport {
    val warningsDiffFile = settingKey[File]("")
    val warningsCurrentFile = settingKey[File]("")
    val warningsPreviousFile = settingKey[File]("")
    val warnings = taskKey[Warnings]("")
    val warningsDiff = taskKey[WarningDiff]("")
    val warningsAll = taskKey[Warnings]("")
    val warningsPrevious = taskKey[Option[Warnings]]("")
  }

  import autoImport._

  case class Pos(
    line: Option[Int],
    lineContent: String,
    offset: Option[Int],
    pointer: Option[Int],
    pointerSpace: Option[String],
    sourcePath: Option[String],
    startOffset: Option[Int],
    endOffset: Option[Int],
    startLine: Option[Int],
    startColumn: Option[Int],
    endLine: Option[Int],
    endColumn: Option[Int]
  )

  private[this] def j2s[A](j: java.util.Optional[A]): Option[A] =
    if (j.isPresent) Option(j.get) else None

  private[this] def j2sInt(j: java.util.Optional[Integer]): Option[Int] =
    if (j.isPresent) Option(j.get) else None

  object Pos {
    def fromSbt(p: Position): Pos = Pos(
      line = j2sInt(p.line()),
      lineContent = p.lineContent(),
      offset = j2sInt(p.offset()),
      pointer = j2sInt(p.pointer()),
      pointerSpace = j2s(p.pointerSpace()),
      sourcePath = j2s(p.sourcePath()),
      startOffset = j2sInt(p.startOffset()),
      endOffset = j2sInt(p.endOffset()),
      startLine = j2sInt(p.startLine()),
      startColumn = j2sInt(p.startColumn()),
      endLine = j2sInt(p.endLine()),
      endColumn = j2sInt(p.endColumn())
    )
  }

  case class Warning(message: String, position: Pos) {
    override def toString = this.toJsonString
  }

  object Warning {
    def fromSbt(p: Problem): Warning = {
      Warning(
        message = p.message(),
        position = Pos.fromSbt(p.position())
      )
    }
    implicit val instance: JsonFormat[Warning] = {
      implicit val position: JsonFormat[Pos] = BasicJsonProtocol.caseClass12(Pos.apply, Pos.unapply)(
        "line",
        "lineContent",
        "offset",
        "pointer",
        "pointerSpace",
        "sourcePath",
        "startOffset",
        "endOffset",
        "startLine",
        "startColumn",
        "endLine",
        "endColumn"
      )
      caseClass2(Warning.apply, Warning.unapply)(
        "message",
        "position"
      )
    }
  }

  type WarningDiff = List[String]
  type Warnings = Seq[Warning]
  def loadWarningsFromJsonFile(file: File): Warnings = {
    val unbuilder = new Unbuilder(sjsonnew.support.scalajson.unsafe.Converter.facade)
    val json = sjsonnew.support.scalajson.unsafe.Parser.parseFromFile(file).get
    implicitly[JsonFormat[Warnings]].read(Option(json), unbuilder)
  }

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
          Warning.fromSbt(p)
      }.toSeq
    }
  }

  private[this] def dir = "warnings"

  override def buildSettings: Seq[Def.Setting[?]] = Def.settings(
    warningsCurrentFile := {
      (LocalRootProject / target).value / dir / "warnings.json"
    },
    warningsDiffFile := {
      (LocalRootProject / target).value / dir / "warnings.diff"
    },
    warningsPreviousFile := {
      (LocalRootProject / target).value / dir / "warnings-previous.json"
    },
    warningsPrevious := {
      val f = warningsPreviousFile.value
      val s = streams.value
      if (f.isFile) {
        Some(loadWarningsFromJsonFile(f))
      } else {
        s.log.warn(s"$f does not exists")
        None
      }
    },
    warningsDiff := Def.taskDyn {
      warningsPrevious.?.value.flatten match {
        case Some(previous) =>
          Def.task[WarningDiff] {
            val current = warningsAll.value
            val order: Ordering[Warning] = Ordering.by(x => (x.position.sourcePath, x.position.line, x.message))
            def format(warnings: Warnings): Seq[String] = {
              warnings
                .sorted(order)
                .flatMap(a => List(a.position.sourcePath.getOrElse(""), a.position.lineContent, a.message))
            }

            val result = IO.withTemporaryDirectory { dir =>
              val c = dir / "current.txt"
              val p = dir / "previous.txt"
              IO.writeLines(c, format(current))
              IO.writeLines(p, format(previous))
              sys.process
                .Process(
                  command = Seq("diff", p.getAbsolutePath, c.getAbsolutePath),
                  cwd = Some(dir)
                )
                .lineStream_!
                .toList
            }

            warningsDiffFile.?.value match {
              case Some(diffFile) =>
                streams.value.log.info(s"write to ${diffFile}")
                IO.writeLines(diffFile, result)
              case _ =>
                streams.value.log.warn(s"${warningsDiffFile.key.label} undefined")
            }
            result
          }
        case None =>
          val s = streams.value
          Def.task[WarningDiff] {
            s.log.warn(s"empty ${warningsPrevious.key.label}")
            Nil
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
