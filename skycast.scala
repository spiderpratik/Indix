/**
 * Created by codi on 5/6/14.
 * SkyCast Coding Challenge
 */

class skycast {

  val INVALID = -100

  def directUsingNumbers(channel: Int): Int = {
    if (channel == 0) 0
    else directUsingNumbers(channel / 10) + 1
  }

  def upDownWithoutBack(channelWhitelist: List[Int], current: Int, next: Int): Int = {
    val direct = math.abs(channelWhitelist.indexOf(current) - channelWhitelist.indexOf(next))
    val loopAround = channelWhitelist.length - direct
    math.min(direct, loopAround)
  }

  def upDownWithBack(channelWhitelist: List[Int], back: Int, next: Int): Int = {
    val direct = math.abs(channelWhitelist.indexOf(back) - channelWhitelist.indexOf(next))
    val loopAround = channelWhitelist.length - direct
    math.min(direct, loopAround) + 1
  }

  def main(minchannel : Int, maxchannel: Int, channelBlackList: List[Int], channelsToVisit: List[Int]) : Int = {
    val availableChannels : List[Int] = (minchannel until (maxchannel+1)).toList
    val channelWhiteList = availableChannels diff channelBlackList
    totalLeastButtons(channelsToVisit, channelWhiteList, INVALID, INVALID)
  }

  def totalLeastButtons(channelsToVisit : List[Int], channelWhiteList: List[Int], currentChannel: Int, backChannel: Int) : Int = {
    channelsToVisit match {
      case Nil => 0
      case head::Nil => leastButtons(currentChannel, backChannel, head, channelWhiteList)
      case head::head2::rest if head == head2 => totalLeastButtons(head2::rest, channelWhiteList, currentChannel, backChannel)
      case head::head2::rest if head != head2 => leastButtons(currentChannel, backChannel, head, channelWhiteList) + totalLeastButtons(head2::rest, channelWhiteList, head, currentChannel)
    }
  }

  def leastButtons(currentChannel: Int, backChannel: Int, nextChannel: Int, channelWhiteList : List[Int]) : Int = {
    val updown =
      if (currentChannel != INVALID) upDownWithoutBack(channelWhiteList, currentChannel, nextChannel)
      else -INVALID
    val backupdown =
      if (backChannel != INVALID) upDownWithBack(channelWhiteList, backChannel, nextChannel)
      else -INVALID
    val directnumbers = directUsingNumbers(nextChannel)
    math.min(math.min(updown, backupdown), directnumbers)
  }
}