package spice.http.server.rest

import spice.http.content.FormDataContent

/**
 * Drop-in convenience class to wrap around an existing Request object and give back `FormDataContent` when available.
 */
trait MultipartRequest {
  def content: FormDataContent

  def withContent(content: FormDataContent): MultipartRequest
}