package com.tiny
package dto.request

import spray.json.{DefaultJsonProtocol, JsValue, RootJsonReader}
import `implicit`._

case class ChangePathTownMapRequest(data: (String, Map[String, Double]))

object ChangePathTownMapRequest extends DefaultJsonProtocol with BaseRequest[ChangePathTownMapRequest] {
  implicit object formatter extends RootJsonReader[ChangePathTownMapRequest] {
    override def read(json: JsValue): ChangePathTownMapRequest = ChangePathTownMapRequest(json.convertTo[Map[String, Map[String, Double]]].toSeq.head)
  }
  override def isRequestValid(request: ChangePathTownMapRequest): Boolean =
    request.data._1.trim.nonEmpty && request.data._2.isStringKeyMapValid
}
