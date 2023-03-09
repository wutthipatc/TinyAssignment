package com.tiny

import route.ShortestDistanceRoute
import service.ShortestDistanceService

import akka.actor.ActorSystem
import akka.http.scaladsl.Http

object Main {
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem = ActorSystem("shortest-distance")
    val service = new ShortestDistanceService()
    val route = new ShortestDistanceRoute(service).createRoute
    Http().newServerAt("localhost", 8080).bind(route)
  }
}
