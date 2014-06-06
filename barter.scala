/**
 * Created by india on 5/6/14.
 * Barter Coding Problem
 */
class barter {

  class fraction(numerator: Int, denominator: Int)
  {
    val hcf = HCF(numerator, denominator)
    val num = numerator / hcf
    var den = denominator / hcf

    def this() = this(1,1)

    def this(fr: fraction) = this(fr.num, fr.den)

    def HCF(a: Int, b: Int): Int = {
      if(a%b == 0) b
      else HCF(b, a % b)
    }

    def simplify(): fraction = {
      new fraction(num, den)
    }

    def multiply(fr: fraction): fraction = {
      new fraction(fr.num * this.num, fr.den * this.den)
    }

    def divide(fr: fraction): fraction = {
      new fraction(fr.den * this.num, fr.num * this.den)
    }
  }

  val DIMENSION = 100 // maximum dimension of the 2D square matrix that represents the graph
                                          // This 2D matrix is implemented using 1D Array
  val INVALID = new fraction(-10, 1)

  def addNode(matrix: Array[fraction], f1: fraction, n1: Int, f2: fraction, n2: Int): Array[fraction] = {
    matrix(n1*DIMENSION+n2) = f1.divide(f2)
    matrix(n2*DIMENSION+n1) = f2.divide(f1)
    matrix
  }

  def graphDFS(from: Int, to: Int, oldVisited: List[Int],matrix: Array[fraction], curr_frac: fraction): fraction = {
    val visited: List[Int] = oldVisited:::List(from)
    (from, to) match{
      case (_,_) if matrix(from*DIMENSION+to) != null =>
        curr_frac.multiply(matrix(from * DIMENSION + to))
      case (_,_) =>
        for(i <- from*DIMENSION to (from+1)*DIMENSION) {
          if (matrix(i) != null && visited.find(x => x == i%DIMENSION).isEmpty) {
            return graphDFS(i%DIMENSION, to, visited, matrix, curr_frac.multiply(matrix(i)))
          }
        }
       INVALID
    }
  }
  def itemMapIndex(input:List[String]):Map[String,Int] ={
    val output = mainFunction(input).distinct
    val listindex =1 to output.length
    (output zip listindex).toMap
  }

  def indexMapItem(input:List[String]):Map[Int,String] ={
    val output = mainFunction(input).distinct
    val listindex =1 to output.length
    (listindex zip output ).toMap
  }

  def mainFunction(input:List[String]):List[String] ={
    input match{
      case a::Nil =>
        val tmp = a.split(" ")
        if(tmp(0) == "!"){
          List(tmp(2),tmp(5))
        }
        else
          List("")
      case a::b =>
        val tmp = a.split(" ")
        if(tmp(0) == "!") {
          List(tmp(2),tmp(5)) ++ mainFunction(b)
        }
        else
          List("")
    }
  }

  def generateList(input:List[String]):List[List[String]] ={
    input match{
      case a::Nil =>
        val tmp = a.split(" ")
        if(tmp(0) == "!"){
          List(List(tmp(1) ,tmp(2), tmp(4), tmp(5)))
        }
        else
          Nil
      case a::b =>
        val tmp = a.split(" ")
        if(tmp(0) == "!"){
          List(List(tmp(1) ,tmp(2), tmp(4), tmp(5))) ++ generateList(b)
        }
        else
          Nil ++ generateList(b)
    }
  }

  def generateQuestionList(input:List[String]):List[List[String]] ={
    input match{
      case a::Nil =>
        val tmp = a.split(" ")
        if(tmp(0) == "?"){
          List(List(tmp(1), tmp(3)))
        }
        else
          Nil
      case a::b =>
        val tmp = a.split(" ")
        if(tmp(0) == "?"){
          List(List(tmp(1) ,tmp(3))) ++ generateQuestionList(b)
        }
        else
          Nil ++ generateQuestionList(b)
    }
  }

  def main(inputList: List[String]): String  = {
    var matrix: Array[fraction] = new Array[fraction](12000)
    var finalStr = ""

    val mapOfitemsToIndex = itemMapIndex(inputList)
    val mapOfIndexToItems = indexMapItem(inputList)
    val informationList = generateList(inputList)
    val QuestionList = generateQuestionList(inputList)
    for(i <- 1 to informationList.length)
      matrix = addNode(matrix, new fraction(informationList(i-1)(0).toInt ,1), mapOfitemsToIndex(informationList(i-1)(1)),new fraction(informationList(i-1)(2).toInt,1), mapOfitemsToIndex(informationList(i-1)(3)))

    for( i <- 0 to QuestionList.length-1) {
      val from = mapOfitemsToIndex(QuestionList(i)(0))
      val to = mapOfitemsToIndex(QuestionList(i)(1))
      val visited = List[Int]()
      val final_frac_not_simplified = graphDFS(from, to, visited, matrix, new fraction(1, 1))
      val final_frac = final_frac_not_simplified.simplify()
      if(final_frac.num > 0)
       finalStr  = finalStr + "__" + (final_frac.num + " " + mapOfIndexToItems(from) + " = " + final_frac.den + " " + mapOfIndexToItems(to))
      else finalStr = finalStr + "__" + ("? " + mapOfIndexToItems(from) + " = ? " + mapOfIndexToItems(to))
    }
    finalStr
  }
}
