package com.tiny
package dto.request

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.language.implicitConversions
import `implicit`._

case class CreateTownMapRequest(map: Seq[Map[String, Map[String, Double]]])

object CreateTownMapRequest extends DefaultJsonProtocol with BaseRequest[CreateTownMapRequest] {
  implicit val formatter: RootJsonFormat[CreateTownMapRequest] = jsonFormat1(CreateTownMapRequest.apply)
  override def isRequestValid(request: CreateTownMapRequest): Boolean =
    request.map.nonEmpty && request.map.forall(_.isStringKeyMapValid)
}