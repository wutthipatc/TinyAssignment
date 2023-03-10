package com.tiny
package dto.response

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Rejection, StandardRoute}
import org.slf4j.Logger
import spray.json.{DefaultJsonProtocol, JsNull, JsObject, JsString, JsonWriter, RootJsonWriter}

import scala.util.Try

case class BaseResponse[T: JsonWriter](dataOption: Option[T], errorMessageOption: Option[String], statusCodeOption: Option[StatusCode] = None) {
  private def isSuccess: Boolean = dataOption.isDefined && errorMessageOption.isEmpty
  def getCompleteRoute: StandardRoute =
    if (isSuccess) statusCodeOption.map(complete(_, this)).getOrElse(complete(StatusCode.int2StatusCode(200), this))
    else statusCodeOption.map(complete(_, this)).getOrElse(complete(StatusCode.int2StatusCode(500), this))
}

object BaseResponse extends DefaultJsonProtocol {
  def getSuccessResponse[T: JsonWriter](data: T, statusCodeOption: Option[StatusCode] = None): BaseResponse[T] = BaseResponse(Some(data), None, statusCodeOption)
  def getErrorResponse[T: JsonWriter](errorMessage: String, statusCodeOption: Option[StatusCode] = None): BaseResponse[T] = BaseResponse(None, Some(errorMessage), statusCodeOption)
  implicit def writer[T: JsonWriter]: RootJsonWriter[BaseResponse[T]] = (obj: BaseResponse[T]) => JsObject(
    "data" -> obj.dataOption.map(implicitly[JsonWriter[T]].write(_)).getOrElse(JsNull),
    "errorMessage" -> obj.errorMessageOption.map(JsString(_)).getOrElse(JsNull)
  )
  def recoverWithErrorBaseResponse[T: JsonWriter](logger: Logger, errorMsgPrefix: String)(respWithPossibleError: => BaseResponse[T]): BaseResponse[T] = {
    def errorMsg(t: Throwable) = s"$errorMsgPrefix ${t.getMessage}"
    Try(respWithPossibleError).recover{ case t =>
      logger.error(errorMsg(t), t)
      getErrorResponse[T](errorMsg(t))
    }.get
  }

  def getValidationErrorCompleteRoute[T: JsonWriter](errMsg: String): StandardRoute =
    getErrorResponse[T](errMsg, Some(StatusCode.int2StatusCode(400))).getCompleteRoute

  def getDefaultRejectionCompleteRoute[T: JsonWriter](seq: Seq[Rejection]): StandardRoute =
    getErrorResponse[T](seq.mkString, Some(StatusCode.int2StatusCode(400))).getCompleteRoute

}