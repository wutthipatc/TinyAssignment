package com.tiny
package dto.request

import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import `implicit`._

case class AddPathTownMapRequest(map: Seq[Map[String, Map[String, Double]]])

object AddPathTownMapRequest extends DefaultJsonProtocol with BaseRequest[AddPathTownMapRequest] {
  implicit val formatter: RootJsonFormat[AddPathTownMapRequest] = jsonFormat1(AddPathTownMapRequest.apply)
  override def isRequestValid(request: AddPathTownMapRequest): Boolean =
    request.map.nonEmpty && request.map.forall(_.isStringKeyMapValid)
}
