/**
 * Created by codi on 5/6/14.
 * SkyCast Coding Challenge
 */

def directUsingNumbers(channel : Int) : Int = {
  if (channel == 0) 0
  else directUsingNumbers(channel/10) + 1
}

def upDownWithoutBack(channelWhitelist : List[Int], current : Int, next : Int) : Int = {
  val direct = math.abs(channelWhitelist.indexOf(current) - channelWhitelist.indexOf(next))
  val loopAround = channelWhitelist.length - direct
  return math.min(direct, loopAround)
}

def upDownWithBack(channelWhitelist : List[Int], back : Int, next : Int) : Int = {
  val direct = math.abs(channelWhitelist.indexOf(back) - channelWhitelist.indexOf(next))
  val loopAround = channelWhitelist.length - direct
  return math.min(direct, loopAround) + 1
}

def leastButtonPress(channels : List[Int]) : Int = {

}