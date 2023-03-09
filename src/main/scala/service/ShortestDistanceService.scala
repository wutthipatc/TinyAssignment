package com.tiny
package service

import `implicit`._
import dto.response.{BaseResponse, CalculateDistanceResponse}
import model.{Path, ResultState, ShortestPath, TownMap}

import akka.http.scaladsl.model.StatusCode
import org.slf4j.LoggerFactory
import spray.json.DefaultJsonProtocol._

import scala.annotation.tailrec
import scala.collection.mutable

class ShortestDistanceService(var townMap: TownMap = TownMap.empty){
  private type NodePairDistanceValue = (String, (String, Double))
  private val logger = LoggerFactory.getLogger(classOf[ShortestDistanceService])
  private def createTownMapRaw(mapList: List[(String, Map[String, Double])]): Unit =
    townMap = TownMap.fromRequestMapList(mapList)
  private def isSameNodePair(elem1: NodePairDistanceValue, elem2: NodePairDistanceValue): Boolean =
    elem1._1 == elem2._2._1 && elem1._2._1 == elem2._1
  private def nodePairValuesContain(list: List[NodePairDistanceValue], elem: NodePairDistanceValue): Boolean =
    list.exists(isSameNodePair(_, elem))
  def createTownMap(mapList: List[(String, Map[String, Double])]): BaseResponse[String] = {
    logger.info(s"ShortestDistanceService::createTownMap with mapList: $mapList")
    BaseResponse.recoverWithErrorBaseResponse(logger, "ShortestDistanceService::createTownMap error occurred with message:")
    {
      createTownMapRaw(mapList)
      BaseResponse.getSuccessResponse("Created town map successfully")
    }
  }
  def changePathValue(pathToChange: (String, Map[String, Double])): BaseResponse[String] = {
    logger.info(s"ShortestDistanceService::changePathValue with pathToChange: $pathToChange")
    BaseResponse.recoverWithErrorBaseResponse(logger, "ShortestDistanceService::changePathValue error occurred with message:")
    {
      val currentNodePairDistanceValues: List[NodePairDistanceValue] = townMap.toMapList
        .flatMap { case (name, map) => map.toList.map(tuple => name -> tuple) }
      val pathToChangeNodePairDistanceValues: List[NodePairDistanceValue] = pathToChange._2.toList.map(tuple => pathToChange._1 -> tuple)
      val reversePathToChangeNodePairDistanceValues: List[NodePairDistanceValue] = pathToChangeNodePairDistanceValues.map { case (name, (target, distance)) => target -> (name, distance) }
      val removedNodePairDistanceValues: List[NodePairDistanceValue] = currentNodePairDistanceValues.filter(tuple => !nodePairValuesContain(pathToChangeNodePairDistanceValues ++ reversePathToChangeNodePairDistanceValues, tuple))
      if (currentNodePairDistanceValues.length != removedNodePairDistanceValues.length) {
        createTownMapRaw(removedNodePairDistanceValues.groupBy(_._1).map { case (key, list) => key -> list.map(_._2).toMap }.toList :+ pathToChange)
        BaseResponse.getSuccessResponse("Changed path value successfully")
      }
      else {
        val errorMsg = "ShortestDistanceService::changePathValue not found some requested town node"
        logger.error(errorMsg)
        BaseResponse.getErrorResponse[String](
          errorMsg,
          Some(StatusCode.int2StatusCode(400))
        )
      }
    }
  }
  def addPath(pathToAdd: List[(String, Map[String, Double])]): BaseResponse[String] = {
    logger.info(s"ShortestDistanceService::addPath with pathToAdd: $pathToAdd")
    BaseResponse.recoverWithErrorBaseResponse(logger, "ShortestDistanceService::changePathValue error occurred with message:")
    {
      val currentNodePairDistanceValues: List[NodePairDistanceValue] = townMap.toMapList
        .flatMap { case (name, map) => map.toList.map(tuple => name -> tuple) }
      val pathToAddNodePairDistanceValues: List[NodePairDistanceValue] = pathToAdd
        .flatMap{ case (name, map) => map.toList.map(tuple => name -> tuple) }
      if (pathToAddNodePairDistanceValues.exists(nodePairValuesContain(currentNodePairDistanceValues, _))) {
        val errorMsg = "ShortestDistanceService::addPath duplicated path could not be added"
        logger.error(errorMsg)
        BaseResponse.getErrorResponse[String](
          errorMsg,
          Some(StatusCode.int2StatusCode(400))
        )
      }
      else {
        createTownMapRaw(townMap.toMapList ++ pathToAdd)
        BaseResponse.getSuccessResponse("Added path successfully")
      }
    }
  }
  def findShortestDistanceFromTo(from: String, to: String): BaseResponse[CalculateDistanceResponse] = {
    logger.info(s"ShortestDistanceService::findShortestDistanceFromTo with from: $from, to: $to")
    BaseResponse.recoverWithErrorBaseResponse(logger, "ShortestDistanceService::findShortestDistanceFromTo error occurred with message:")
    {
      val initResultState = ResultState.init(from, townMap.townNodes.map(_.name).filter(_ != from))
      val finalResultState = findShortestDistanceRecurse(from, to, List.empty, mutable.PriorityQueue.from(initResultState.shortestPathMap.values), initResultState)
      val result = finalResultState.calculateDistance(to)
      BaseResponse.getSuccessResponse(CalculateDistanceResponse(result))
    }
  }
  @tailrec
  private def findShortestDistanceRecurse(from: String, to: String, visitedTowns: List[String], unvisitedTowns: mutable.PriorityQueue[ShortestPath], resultState: ResultState): ResultState = {
    if (visitedTowns.contains(to) || visitedTowns.length == resultState.getMapSize) resultState
    else {
      val shortestPath = unvisitedTowns.dequeue()
      val unvisitedNeighbours: List[Path] = townMap.townNodes
        // Because we only do enqueue on the PQ (No remove specific item on PQ provided), we need to filter out the already visited town
        .filter(townNode => townNode.name == shortestPath.townName && !visitedTowns.contains(townNode.name))
        .flatMap(_.neighbours)
        .filter(path => !visitedTowns.contains(path.target))
      val updatedResultState: ResultState = unvisitedNeighbours.foldLeft(resultState) ((acc, path) => path match {
        case x if (shortestPath.distance + x.distance).lessThanWithAcceptedError(resultState.shortestPathMap(x.target).distance) =>
          unvisitedTowns.enqueue(ShortestPath(x.target, shortestPath.distance + x.distance, Some(shortestPath.townName)))
          acc.update(x.target, shortestPath.distance + x.distance, shortestPath.townName)
        case _ => acc
      })
      findShortestDistanceRecurse(
        from,
        to,
        visitedTowns :+ shortestPath.townName,
        unvisitedTowns,
        updatedResultState
      )
    }
  }
}
