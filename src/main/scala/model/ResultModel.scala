package com.tiny
package model
import `implicit`._

sealed trait ResultModel
case class ResultState(fromTownName: String, shortestPathMap: Map[String, ShortestPath]) extends ResultModel {
  def update(townName: String, newDistance: Double, newPrevious: String): ResultState =
    ResultState(fromTownName, shortestPathMap + (townName -> ShortestPath(townName, newDistance, Some(newPrevious))))
  def calculateDistance(target: String): Double = shortestPathMap(target).distance
  def getMapSize: Int = shortestPathMap.size
}
case class ShortestPath(townName: String, distance: Double, previousTownName: Option[String]) extends ResultModel

object ResultState {
  def init(from: String, otherTowns: List[String]): ResultState =
    ResultState(
      from,
      Map.from((from -> ShortestPath(from, 0, Option.empty)) +: otherTowns.map(townName => townName -> ShortestPath(townName, Double.MaxValue, Option.empty)))
    )
}
object ShortestPath {
  implicit val order: Ordering[ShortestPath] = Ordering.fromLessThan[ShortestPath]((s1, s2) => s1.distance.lessThanWithAcceptedError(s2.distance)).reverse
}
