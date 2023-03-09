package com.tiny
package model

sealed trait InputModel
case class TownMap(townNodes: List[TownNode]) extends InputModel {
  def toMapList: List[(String, Map[String, Double])] =
    townNodes.map(townNode => townNode.name -> townNode.neighbours.map(path => path.target -> path.distance).toMap)
}
case class Path(distance: Double, target: String) extends InputModel
case class TownNode(name: String, neighbours: List[Path]) extends InputModel

object TownMap {
  def empty: TownMap = TownMap(List.empty)
  def fromRequestMapList(mapList: List[(String, Map[String, Double])]): TownMap =
    TownMap(mapList
      .flatMap(TownNode.fromRequestedMapElement)
      .distinct
      .groupBy(_.name)
      .map{case (townName, townNodes) => TownNode(townName, townNodes.flatMap(_.neighbours))}
      .toList
    )
}

object TownNode {
  def fromRequestedMapElement(elem: (String, Map[String, Double])): List[TownNode] =
    elem._2.flatMap{case (requestedTarget, requestedDistance) =>
      List(
        TownNode(elem._1, List(Path(requestedDistance, requestedTarget))),
        TownNode(requestedTarget, List(Path(requestedDistance, elem._1)))
      )
    }.toList
}
