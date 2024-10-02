package spice.http.server.rest

import fabric.define.DefType
import fabric.obj
import fabric.rw._
import spice.http.Headers

import java.io.File

case class FileUpload(fileName: String, file: File, headers: Headers)

object FileUpload {
  implicit val rw: RW[FileUpload] = RW.from[FileUpload](
    r = f => obj().withReference(f),
    w = json => json.reference.getOrElse(throw new RuntimeException("No reference for FileUpload")).asInstanceOf[FileUpload],
    d = DefType.Obj(Some("spice.http.server.rest.FileUpload"))
  )
}