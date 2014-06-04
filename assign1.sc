// #1
def last(l1 : List[Int]) : Int = {
  l1 match {
    case Nil => throw new NoSuchElementException
    case head::Nil => head
    case head::rest => last(rest)
  }
}

// #2
def penultimate(l2 : List[Int]) : Int = {
  l2 match {
    case Nil => throw new NoSuchElementException
    case head::Nil => throw new NoSuchElementException
    case head::head2::Nil => head
    case head::head2::rest => penultimate(head2+:rest)
  }
}

// #3
def reverse(l3 : List[Int]) : List[Int] = {
  l3 match {
    case Nil => Nil
    case head::rest => reverse(rest) :+ head
  }
}

// #4
def compress(l4 : List[Int]) : List[Int] = {
  l4 match {
    case Nil => Nil
    case head::Nil => List(head)
    case head::head2::rest => if(head == head2) compress(head2::rest)
    else head::compress(head2::rest)
  }
}

// #5
def duplicateSingleton(N : Int, num : Int) : List[Int] = {
  N match {
    case 0 => Nil
    case _ => List(num):::duplicateSingleton(N-1, num)
  }
}

def duplicateN(N : Int, l5 : List[Int]) : List[Int] = {
  (N,l5) match {
    case (_, Nil) => Nil
    case (N, head::rest) => duplicateSingleton(N, head):::duplicateN(N, rest)
  }
}

// #6
def delete(N : Int, index : Int, l6 : List[Int]) : List[Int] = {
  (N, index, l6 ) match {
    case (0, _, _) => throw new NoSuchElementException
    case (_, _, Nil) => Nil
    case (N, 1, head::rest) => delete(N, N, rest)
    case (N, index, head::rest) => List(head):::delete(N, index-1, rest)
  }
}

def drop(N : Int, list : List[Int] ) : List[Int] = {
  delete(N, N, list)
}

// #7
def slice(start : Int, end : Int, l7 : List[Int]) : List[Int] = {
  (start, end, l7) match {
    case (_, _, Nil) => Nil
    case (_, 0, _) => Nil
    case (0, _, head::tail) => List(head):::slice(0, end-1, tail)
    case (_, _, head::tail) => slice(start-1, end-1, tail)
  }
}

// #8
def isPrime(num : Int, k : Int): Boolean = {
  (num, k) match {
    case (_, 0) => false
    case (_, 1) => true
    case (_,_) => if (num%k == 0) false
    else isPrime(num, k-1)
  }
}

def listPrimesinRange(start : Int, end : Int) : List[Int] = {
  if(start > end)
    Nil

  else if(isPrime(start ,start/2 ))
    List(start):::listPrimesinRange(start+1, end)

  else listPrimesinRange(start+1, end)
}

// #9
def multiplesThreeFive(N : Int ) : Int = {
  if(N == 0)
    0

  else if(N%3 == 0 || N%5 == 0)
    N + multiplesThreeFive(N-1)

  else multiplesThreeFive(N-1)
}

// #10
def fullWords(num : Int) : List[String] = {
  val map = Map(0 -> "zero", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five",
    6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine")

  if (num < 10) List(map(num))
  else fullWords(num/10) :+ map(num%10)
}

// #11
def numberOfPaths(rows : Int, columns : Int) : Int = {
  if (rows == 1 || columns == 1)
    rows+columns
  else
    numberOfPaths(rows-1, columns) + numberOfPaths(rows, columns-1)
}

// #12
def factorial(num : Int) : Int = {
  if (num == 0) 1
  else num*factorial(num-1)
}

def lexicographic(pos : Int, list : List[Int]) : List[Int] = {
  if (list.length == 0 || list.length == 1) list
  else {
    val fac = factorial(list.length-1)
     List(list(pos/fac)) ::: lexicographic(pos%fac, list diff List(list(pos/fac)))
  }
}



assert ( last ( List ( 1,2,3,4,5 ) ) == 5 )
assert ( last ( List ( 1,2,3,4,5,6 ) ) == 6 )
assert ( last ( List ( 1,2,3,4,5,6,6,5 ) ) == 5 )
assert ( last ( List ( 1,2,3,4,5,7,7 ) ) == 7 )

assert( penultimate( List( 1,2,3,4,5 ) ) == 4)
assert( penultimate( List( 1,2,3,4,5,6 ) ) == 5)
assert( penultimate( List( 1,2,3,4,5,6,6,5 ) ) == 6)
assert( penultimate( List( 1,2,3,4,5,7,7 ) ) == 7)

assert( reverse( List( 1,2,3,4,5 ) ) ==   List(5,4,3,2,1) )
assert( reverse( List( 1,2,3,4,5,6 ) ) == List(6,5,4,3,2,1) )
assert( reverse( List( 1,2,3,4,5,6,6,5 ) ) == List(5,6,6,5,4,3,2,1) )
assert( reverse( List( 1,2,3,4,5,7,7 ) ) == List(7,7,5,4,3,2,1) )

assert( compress(List(1,1,1,1)) == List(1))
assert( compress(List(1,2,2,3,3,1,1,1)) == List(1,2,3,1))
assert( compress(List(1,1,2,2,1,1,2,2)) == List(1,2,1,2))
assert( compress(List(3,3,3,3,3,3,3,3,31,1,1)) == List(3,31,1))

assert( duplicateN (3,List(1,1)) == List(1,1,1,1,1,1))
assert( duplicateN (2,List(1,2,3)) == List(1,1,2,2,3,3))
assert( duplicateN (4,List(2,1,2)) == List(2,2,2,2,1,1,1,1,2,2,2,2))
assert( duplicateN (5,List(2)) == List(2,2,2,2,2))

assert(drop(18,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
assert(drop(10,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14))
assert(drop(5,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14))
assert(drop(6,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14))

assert(slice(0,15,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
assert(slice(1,10,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(2, 3, 4, 5, 6, 7, 8, 9, 10))
assert(slice(3,5,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(4, 5))
assert(slice(3,4,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(4))

assert(listPrimesinRange(4,13) == List(5,7,11,13))
assert(listPrimesinRange(7,31) == List(7,11,13,17,19,23,29,31))
assert(listPrimesinRange(4,31) == List(5,7,11,13,17,19,23,29,31))
assert(listPrimesinRange(1,10) == List(2,3,5,7))

assert(multiplesThreeFive ( 999 ) == 233168 )
assert(multiplesThreeFive( 10 ) == 33 )
assert(multiplesThreeFive( 20 ) == 98 )
assert(multiplesThreeFive( 30 ) == 225 )

//assert( numberOfPaths ( 10, 10 ) == 184756)
assert( numberOfPaths ( 5, 5 ) == 252)
assert( numberOfPaths ( 2, 2 ) == 6)
assert( numberOfPaths ( 3, 3 ) == 20)

assert( fullWords ( 100 ) == List( "one","zero","zero" ) )
assert( fullWords ( 132 ) == List( "one","three","two" ) )
assert( fullWords ( 1432 ) == List( "one","four","three","two" ) )
assert( fullWords ( 4312 ) == List( "four","three","one","two" ) )

val l = List(0,1,2,3,4,5,6,7,8,9)
assert(lexicographic(1000000,l) == List(2,7,8,3,9,1,5,6,0,4))
assert(lexicographic(999999,l) == List(2,7,8,3,9,1,5,4,6,0))
assert(lexicographic(1,l) == List(0,1,2,3,4,5,6,7,9,8))
assert(lexicographic(99999,l) == List(0,3,5,8,9,2,6,4,7,1))
assert(lexicographic(5,l) == List(0,1,2,3,4,5,6,9,8,7))

