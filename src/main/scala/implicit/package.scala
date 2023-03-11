package com.tiny

import scala.language.implicitConversions

package object `implicit` {
  private val DEFAULT_ACCEPTED_ERROR = 0.00001
  case class DoubleWithErrorComparing(value: Double) extends AnyVal {
    def lessThanWithAcceptedError(other: Double, acceptedError: Double = DEFAULT_ACCEPTED_ERROR): Boolean =
      value - other < 0 && Math.abs(value - other) > acceptedError

    def greaterThanWithAcceptedError(other: Double, acceptedError: Double = DEFAULT_ACCEPTED_ERROR): Boolean =
      value - other > 0 && Math.abs(value - other) > acceptedError
  }
  implicit def doubleToDoubleWithErrorComparing(value: Double): DoubleWithErrorComparing =
    DoubleWithErrorComparing(value)

  implicit class ValidateStringKeyMap[T](map: Map[String, T]) {
    def isStringKeyMapValid: Boolean = isNestedStringKeyMapValidRecurse(map)
    private def isNestedStringKeyMapValidRecurse(map: Map[String, T]): Boolean = {
      if (map.isEmpty) false
      else if (map.values.headOption.exists(!_.isInstanceOf[Map[String, T]]))
        map.nonEmpty && map.keys.forall(_.trim.nonEmpty) && map.values.foldLeft(true)((acc, elem) => elem match {
          case x: String => acc && x.trim.nonEmpty
          case x: Double => acc && x.greaterThanWithAcceptedError(0)
          case _ => acc
        })
      // do recursion
      else
        map.keys.forall(_.trim.nonEmpty) &&
          map.values.map(_.asInstanceOf[Map[String, T]]).map(isNestedStringKeyMapValidRecurse).reduce(_ && _)
    }
  }
}
