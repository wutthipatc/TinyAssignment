package com.tiny
package dto.request

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

case class CalculateDistanceRequest(start: String, end: String)

object CalculateDistanceRequest extends DefaultJsonProtocol with BaseRequest[CalculateDistanceRequest] {
  implicit val formatter: RootJsonFormat[CalculateDistanceRequest] = jsonFormat2(CalculateDistanceRequest.apply)
  override def isRequestValid(request: CalculateDistanceRequest): Boolean =
    (for {
      start <- Option(request.start)
      end <- Option(request.end)
    } yield start.trim.nonEmpty && end.trim.nonEmpty) getOrElse false
}
