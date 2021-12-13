package com.github.netfalo.aoc2021

object Day12 extends Problem[Long, Long] {
  type Graph = Map[String, Seq[String]]

  def parseGraph(input: String): Graph = {
    input
      .split('\n')
      .filterNot(_.isBlank)
      .flatMap(line => {
        val Array(left, right) = line.split('-')
        Array((left, right), (right, left))
      })
      .filterNot(_._2 == "start")
      .groupBy(_._1)
      .map { case (node, neighbours) => (node, neighbours.map(_._2)) }
  }

  def findAllRoutesVisitSmallCavesOnce(graph: Graph): Set[Seq[String]] = {
    def dfs(node: String, route: Seq[String], visited: Set[String]): Set[Seq[String]] = {
      val newVisited = visited + node
      val newRoute = route.appended(node)
      val neighboursToVisit =
        graph(node)
          .filterNot(n => visited.contains(n) && n.toLowerCase == n)

      if (node == "end")
        Set(newRoute)
      else if (neighboursToVisit.isEmpty)
        Set()
      else {
        neighboursToVisit
          .flatMap(node => dfs(node, newRoute, newVisited))
          .toSet
      }
    }

    dfs("start", List(), Set())
  }

  def findAllRoutesVisitOneSmallCaveTwice(graph: Graph): Set[Seq[String]] = {
    def dfs(node: String, route: Seq[String], visited: Set[String], visitedOneSmallTwice: Boolean): Set[Seq[String]] = {
      val newVisited = visited + node
      val newRoute = route.appended(node)
      val neighboursToVisit =
        graph(node)
          .filterNot(n => visited.contains(n) && n.toLowerCase == n && visitedOneSmallTwice)

      if (node == "end")
        Set(newRoute)
      else if (neighboursToVisit.isEmpty)
        Set()
      else {
        neighboursToVisit
          .flatMap(node => dfs(node, newRoute, newVisited, visitedOneSmallTwice || node == node.toLowerCase && visited.contains(node)))
          .toSet
      }
    }

    dfs("start", List(), Set(), visitedOneSmallTwice = false)
  }


  override def solveFirstPart(input: String): Long =
    findAllRoutesVisitSmallCavesOnce(parseGraph(input))
      .size

  override def solveSecondPart(input: String): Long =
    findAllRoutesVisitOneSmallCaveTwice(parseGraph(input))
      .size
}
