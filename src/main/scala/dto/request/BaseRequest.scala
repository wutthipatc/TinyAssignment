package com.tiny
package dto.request

trait BaseRequest[-T] {
  def isRequestValid(request: T): Boolean
}
