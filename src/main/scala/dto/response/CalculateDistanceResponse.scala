package com.tiny
package dto.response

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

case class CalculateDistanceResponse(distance: Double)

object CalculateDistanceResponse extends DefaultJsonProtocol {
  implicit val formatter: RootJsonFormat[CalculateDistanceResponse] = jsonFormat1(CalculateDistanceResponse.apply)
}
