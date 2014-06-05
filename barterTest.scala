/**
 * Created by india on 5/6/14.
 * Barter Coding Problem Test
 */

import org.scalatest._

class barterTest extends FlatSpec with Matchers {

  "barter test returns" should "return correct value " in{

    new barter().main(List("! 6 apple = 15 banana","! 47 grape = 9 mango","! 2 banana = 1 grape","? banana = apple","? mango = apple")) should be ("5 banana = 2 apple45 mango = 188 apple")
    new barter().main(List("! 9 cat = 0 dog","! 1 cow = 12 chicken","? chicken = dog"))should be("? chicken = ? dog")
    new barter().main(List("! 1 fork = 1 spoon","! 4 television = 83 knife","! 91 knife = 2 dvdplayer","! 1 microwave = 37 fork","! 7 toaster = 2 microwave","! 2 spoon = 1 knife","? dvdplayer = toaster","? television = microwave")) should be ("74 dvdplayer = 637 toaster74 television = 83 microwave")

  }
}
