package com.tiny
package service

import dto.response.{BaseResponse, CalculateDistanceResponse}
import model.{Path, TownNode}

import akka.http.scaladsl.model.StatusCode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import spray.json.DefaultJsonProtocol._

class ShortestDistanceServiceTest extends AnyFlatSpec with Matchers {
  val inputMapList: List[(String, Map[String, Double])] = List(
    ("A",Map("B" -> 100.0, "C" -> 30.0)),
    ("B",Map("F" -> 300.0)),
    ("C",Map("D" -> 200.0)),
    ("D",Map("E" -> 80.0, "H" -> 90.0)),
    ("E",Map("F" -> 50.0, "G" -> 150.0, "H" -> 30.0)),
    ("F",Map("G" -> 70.0)),
    ("G",Map("H" -> 50.0))
  )
  val expectedTownMapList: List[TownNode] = List(
    TownNode("A", List(Path(100.0, "B"), Path(30.0, "C"))),
    TownNode("B", List(Path(100.0, "A"), Path(300.0, "F"))),
    TownNode("C", List(Path(30.0, "A"), Path(200.0, "D"))),
    TownNode("D", List(Path(200.0, "C"), Path(80.0, "E"), Path(90.0, "H"))),
    TownNode("E", List(Path(80.0, "D"), Path(50.0, "F"), Path(150.0, "G"), Path(30.0, "H"))),
    TownNode("F", List(Path(300.0, "B"), Path(50.0, "E"), Path(70.0, "G"))),
    TownNode("G", List(Path(150.0, "E"), Path(70.0, "F"), Path(50.0, "H"))),
    TownNode("H", List(Path(90.0, "D"), Path(30.0, "E"), Path(50.0, "G")))
  )
  "A ShortestDistanceService" should "be initiated with default empty TownMap" in {
    val service = new ShortestDistanceService()
    service.townMap.townNodes should have length 0
  }
  it should "create town map from map list successfully" in {
    val service = new ShortestDistanceService()
    val response = service.createTownMap(inputMapList)
    val townNodes = service.townMap.townNodes
    response shouldBe BaseResponse.getSuccessResponse("Created town map successfully")
    townNodes should have size expectedTownMapList.size
    townNodes should contain theSameElementsAs expectedTownMapList
  }
  it should "change path value from tuple of String and Map successfully" in {
    val service = new ShortestDistanceService()
    service.createTownMap(inputMapList)
    val response = service.changePathValue("A" -> Map("B" -> 20))
    val townNodeAOption = service.townMap.townNodes.find(_.name == "A")
    val townNodeBOption = service.townMap.townNodes.find(_.name == "B")
    response shouldBe BaseResponse.getSuccessResponse("Changed path value successfully")
    townNodeAOption shouldBe defined
    townNodeBOption shouldBe defined
    townNodeAOption.get.neighbours should not contain (Path(100, "B"))
    townNodeAOption.get.neighbours should contain (Path(20, "B"))
    townNodeBOption.get.neighbours should not contain (Path(100, "A"))
    townNodeBOption.get.neighbours should contain (Path(20, "A"))
  }
  it should "change path value failed with NON-exist town name" in {
    val service = new ShortestDistanceService()
    service.createTownMap(inputMapList)
    val response = service.changePathValue("A" -> Map("Z" -> 10))
    val townNodeAOption = service.townMap.townNodes.find(_.name == "A")
    response shouldBe BaseResponse.getErrorResponse[String](
      "ShortestDistanceService::changePathValue not found some requested town node",
      Some(StatusCode.int2StatusCode(400))
    )
    townNodeAOption shouldBe defined
    townNodeAOption.get.neighbours should not contain (Path(10, "Z"))
  }
  it should "add path from list of tuple String and Map successfully for exist Town" in {
    val service = new ShortestDistanceService()
    service.createTownMap(inputMapList)
    val response = service.addPath(List("A" -> Map("D" -> 50)))
    val townNodeAOption = service.townMap.townNodes.find(_.name == "A")
    val townNodeDOption = service.townMap.townNodes.find(_.name == "D")
    response shouldBe BaseResponse.getSuccessResponse("Added path successfully")
    townNodeAOption shouldBe defined
    townNodeDOption shouldBe defined
    townNodeAOption.get.neighbours should contain (Path(50, "D"))
    townNodeDOption.get.neighbours should contain (Path(50, "A"))
  }
  it should "add path from list of tuple String and Map successfully for NON-exist town" in {
    val service = new ShortestDistanceService()
    service.createTownMap(inputMapList)
    val response = service.addPath(List("A" -> Map("I" -> 150)))
    val townNodeAOption = service.townMap.townNodes.find(_.name == "A")
    val townNodeIOption = service.townMap.townNodes.find(_.name == "I")
    response shouldBe BaseResponse.getSuccessResponse("Added path successfully")
    townNodeAOption shouldBe defined
    townNodeIOption shouldBe defined
    townNodeAOption.get.neighbours should contain(Path(150, "I"))
    townNodeIOption.get.neighbours should contain(Path(150, "A"))
  }
  it should "add path from list of tuple String and Map failed with duplicated path" in {
    val service = new ShortestDistanceService()
    service.createTownMap(inputMapList)
    val response = service.addPath(List("A" -> Map("B" -> 150)))
    val townNodes = service.townMap.townNodes
    response shouldBe BaseResponse.getErrorResponse[String](
      "ShortestDistanceService::addPath duplicated path could not be added",
      Some(StatusCode.int2StatusCode(400))
    )
    townNodes should have size (expectedTownMapList.size)
    townNodes should contain theSameElementsAs (expectedTownMapList)
  }
  it should "find shortest path with 0 result on the same start and end town" in {
    val service = new ShortestDistanceService()
    service.createTownMap(inputMapList)
    val response = service.findShortestDistanceFromTo("A", "A")
    response shouldBe BaseResponse.getSuccessResponse(CalculateDistanceResponse(0))
  }
  it should "find shortest path correctly on arbitrary start and end town" in {
    val service = new ShortestDistanceService()
    service.createTownMap(inputMapList)
    val response = service.findShortestDistanceFromTo("A", "F")
    response shouldBe BaseResponse.getSuccessResponse(CalculateDistanceResponse(360))
  }
}
