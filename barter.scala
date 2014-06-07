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

  val DIMENSION = 100 // Maximum dimension of the 2D square matrix that represents the graph
                      // This 2D matrix is implemented using 1D Array
  val INVALID = new fraction(-10, 1)
  val DELIMITER = "__"

  def parseInputData(input: List[String]): List[String] = {
    input match{
      case head::Nil =>
        val splitString = head.split(" ")
        if(splitString(0) == "!"){
          List(splitString(2),splitString(5))
        }
        else
          List("")
      case head::rest =>
        val splitString = head.split(" ")
        if(splitString(0) == "!") {
          List(splitString(2), splitString(5)) ::: parseInputData(rest)
        }
        else
          List("")
    }
  }

  def itemMapIndex(input: List[String]): Map[String,Int] = {
    val output = parseInputData(input).distinct
    val index =1 to output.length
    (output zip index).toMap
  }

  def indexMapItem(input: List[String]): Map[Int,String] = {
    itemMapIndex(input).map(_.swap)
  }

  def generateInfoList(input: List[String]): List[List[String]] = {
    input match{
      case a::Nil =>
        val splitString = a.split(" ")
        if(splitString(0) == "!"){
          List(List(splitString(1) ,splitString(2), splitString(4), splitString(5)))
        }
        else
          Nil
      case a::b =>
        val splitString = a.split(" ")
        if(splitString(0) == "!"){
          List(List(splitString(1) ,splitString(2), splitString(4), splitString(5))) ++ generateInfoList(b)
        }
        else
          Nil ++ generateInfoList(b)
    }
  }

  def generateQuestionList(input: List[String]): List[List[String]] = {
    input match{
      case head::Nil =>
        val splitString = head.split(" ")
        if(splitString(0) == "?"){
          List(List(splitString(1), splitString(3)))
        }
        else
          Nil
      case head::rest =>
        val splitString = head.split(" ")
        if(splitString(0) == "?"){
          List(List(splitString(1) ,splitString(3))) ::: generateQuestionList(rest)
        }
        else
          Nil ++ generateQuestionList(rest)
    }
  }

  def main(inputList: List[String]): String  = {
    var matrix: Array[fraction] = new Array[fraction](DIMENSION*DIMENSION)
    val ItemsToIndex = itemMapIndex(inputList)
    val IndexToItems = indexMapItem(inputList)
    val InformationList = generateInfoList(inputList)
    val QuestionList = generateQuestionList(inputList)
    for (i <- 1 to InformationList.length)
      matrix = addNode(matrix, new fraction(InformationList(i - 1)(0).toInt, 1), ItemsToIndex(InformationList(i - 1)(1)), new fraction(InformationList(i - 1)(2).toInt, 1), ItemsToIndex(InformationList(i - 1)(3)))
    val answer = run(0, QuestionList, IndexToItems, ItemsToIndex, matrix)
    answer
  }

  def run(i: Int, QuestionList: List[List[String]], IndexToItems: Map[Int,String], ItemsToIndex: Map[String,Int], mat: Array[fraction]): String = {
    if (i == QuestionList.length) return ""
    val start = ItemsToIndex(QuestionList(i)(0))
    val end = ItemsToIndex(QuestionList(i)(1))
    val visited = List[Int]()
    val FinalFraction = graphDFS(start, end, visited, mat, new fraction(1, 1)).simplify()
    if(FinalFraction.num > 0)
     DELIMITER + (FinalFraction.num + " " + IndexToItems(start) + " = " + FinalFraction.den + " " + IndexToItems(end)) + run(i+1, QuestionList, IndexToItems, ItemsToIndex, mat)
    else DELIMITER + ("? " + IndexToItems(start) + " = ? " + IndexToItems(end)) + run(i+1, QuestionList, IndexToItems, ItemsToIndex, mat)
  }

  def addNode(matrix: Array[fraction], f1: fraction, n1: Int, f2: fraction, n2: Int): Array[fraction] = {
    matrix(n1*DIMENSION+n2) = f1.divide(f2)
    matrix(n2*DIMENSION+n1) = f2.divide(f1)
    matrix
  }

  def graphDFS(start: Int, end: Int, oldVisited: List[Int],matrix: Array[fraction], curr_frac: fraction): fraction = {
    val visited: List[Int] = oldVisited:::List(start)
    (start, end) match{
      case (_,_) if matrix(start*DIMENSION + end) != null =>
        curr_frac.multiply(matrix(start*DIMENSION + end))
      case (_,_) =>
        for(i <- start*DIMENSION to (start+1)*DIMENSION) {
          if (matrix(i) != null && visited.find(x => x == i%DIMENSION).isEmpty) {
            return graphDFS(i%DIMENSION, end, visited, matrix, curr_frac.multiply(matrix(i)))
          }
        }
        INVALID
    }
  }
}
