package com.tiny
package route

import dto.request.{AddPathTownMapRequest, CalculateDistanceRequest, ChangePathTownMapRequest, CreateTownMapRequest}
import dto.response.BaseResponse
import service.ShortestDistanceService

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, ValidationRejection}
import spray.json.DefaultJsonProtocol._

class ShortestDistanceRoute(shortestDistanceService: ShortestDistanceService) {
  private def errorMessage[T](entity: T) = s"Invalid request body for $entity"
  def createRoute: Route =
    handleRejections {
      case seq if seq.exists(_.isInstanceOf[ValidationRejection]) =>
        Some(BaseResponse.getValidationErrorCompleteRoute[String](seq.find(_.isInstanceOf[ValidationRejection]).map{
          case ValidationRejection(msg, _) => msg
        }.getOrElse("Request validation error")))
      case seq => Some(BaseResponse.getDefaultRejectionCompleteRoute[String](seq))
    } {
      pathPrefix("api" / "shortest-distance")(concat(
        post {
          entity(as[CreateTownMapRequest]) { createTownMapReq =>
            validate(CreateTownMapRequest.isRequestValid(createTownMapReq), errorMessage(createTownMapReq)) {
              shortestDistanceService.createTownMap(createTownMapReq.map.flatMap(_.toList).toList)
                .getCompleteRoute
            }
          }
        }
        ,
        (put & path("change-path")) {
          entity(as[ChangePathTownMapRequest]) { changePathReq =>
            validate(ChangePathTownMapRequest.isRequestValid(changePathReq), errorMessage(changePathReq)) {
              shortestDistanceService.changePathValue(changePathReq.data).getCompleteRoute
            }
          }
        }
        ,
        (put & path("add-path")) {
          entity(as[AddPathTownMapRequest]) { addPathReq =>
            validate(AddPathTownMapRequest.isRequestValid(addPathReq), errorMessage(addPathReq)) {
              shortestDistanceService.addPath(addPathReq.map.flatMap(_.toList).toList)
                .getCompleteRoute
            }
          }
        }
        ,
        (post & path("calculate-distance")) {
          entity(as[CalculateDistanceRequest]) { calculateReq =>
            validate(CalculateDistanceRequest.isRequestValid(calculateReq), errorMessage(calculateReq)) {
              shortestDistanceService.findShortestDistanceFromTo(calculateReq.start, calculateReq.end)
                .getCompleteRoute
            }
          }
        }
      ))
    }
}
