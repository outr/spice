package spice.openapi.generator

import java.io.File
import java.nio.file.Path
import java.nio.file.Files
import scala.io.Source

/**
 * Shared write-with-prune for source generators. Writing emissions is
 * the easy half; the writer also OWNS the generated tree: before
 * writing, it recursively removes previously-generated files under
 * every directory this run emits into — including subdirectories with
 * no emissions of their own. A package whose types were all removed
 * upstream would otherwise keep its stale generated files forever,
 * surfacing later as analyzer breakage far from the removal and
 * misattributed to whatever run finally exposed it.
 *
 * A file counts as generated only when its name carries the
 * generator's file extension AND its content carries the generator's
 * marker comment — hand-authored files are never touched, whatever
 * directory they sit in. Directories emptied by the prune are
 * removed.
 */
object GeneratedSourceWriter {

  def write(sourceFiles: List[SourceFile],
            path: Path,
            fileExtension: String,
            generatedComment: String,
            deleteBeforeWrite: Boolean = true): Unit = {
    if (deleteBeforeWrite) prune(sourceFiles, path, fileExtension, generatedComment)
    sourceFiles.foreach { sf =>
      val filePath = path.resolve(s"${sf.path}/${sf.fileName}")
      Files.createDirectories(filePath.getParent)
      Files.writeString(filePath, sf.source)
    }
  }

  private def prune(sourceFiles: List[SourceFile],
                    path: Path,
                    fileExtension: String,
                    generatedComment: String): Unit = {
    val emittedDirs = sourceFiles.map(_.path).distinct.map(p => path.resolve(p).normalize())
    // A root nested under another root is covered by the outer walk.
    val roots = emittedDirs.filterNot { dir =>
      emittedDirs.exists(other => (other ne dir) && dir.startsWith(other) && dir != other)
    }.distinct
    roots.foreach { root =>
      val rootFile = root.toFile
      rootFile.mkdirs()
      pruneTree(rootFile, fileExtension, generatedComment, isRoot = true)
    }
  }

  /** Depth-first: delete generated files, recurse into subdirectories,
    * then drop any non-root directory the prune emptied. */
  private def pruneTree(dir: File, fileExtension: String, generatedComment: String, isRoot: Boolean): Unit = {
    val children = Option(dir.listFiles()).map(_.toList).getOrElse(Nil)
    children.foreach { file =>
      if (file.isDirectory) pruneTree(file, fileExtension, generatedComment, isRoot = false)
      else if (isGenerated(file, fileExtension, generatedComment)) {
        if (!file.delete()) file.deleteOnExit()
      }
    }
    if (!isRoot && Option(dir.listFiles()).exists(_.isEmpty)) {
      dir.delete()
      ()
    }
  }

  def isGenerated(file: File, fileExtension: String, generatedComment: String): Boolean =
    if (!file.isDirectory && file.getName.toLowerCase.endsWith(fileExtension.toLowerCase)) {
      val s = Source.fromFile(file)
      try s.getLines().exists(_.contains(generatedComment))
      finally s.close()
    } else false
}
