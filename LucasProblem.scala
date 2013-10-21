//LucasProblemCalculator.scala

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.math._
import scala.collection.mutable.ListBuffer

case object CalculateSquares
case class Work(startAt: Int, range: Int, step: Int)
case class ResultValue(value: List[Int])

object LucasProblem {

  def main(args: Array[String]) {
	var workers = 0 // taking number of cores
	var step = 0
	if (args.length != 2) {
		println("Invalid number of inputs")
	}
	else if (args(0).toInt < args(1).toInt) {
		println("Starting range is small")
	}
	else {
		if (args(0).toInt <= 10000) {
			workers = 2
			step = 5
		} else if (args(0).toInt > 10000 && args(0).toInt <= 100000) {
			workers = 4
			step = 10
		} else if (args(0).toInt > 100000 && args(0).toInt < 1000000) {
			workers = 8
			if (args(1).toInt < 80)
				step = 10
			else if(args(1).toInt >= 80)
				step = 5
		} else if (args(0).toInt >= 1000000) {
			workers = 16
			if (args(1).toInt < 80)
				step = 50
			else if(args(1).toInt >= 80)
				step = 40
		}
		findPerfectSquare(workers, args(0).toInt, args(1).toInt,step)
	}
  }
  
  def findPerfectSquare(Workers: Int, endAt: Int, Range: Int, step: Int) {
    
    val system = ActorSystem("system")

    val masterActor = system.actorOf(Props(new Master(Workers, endAt, Range, step)),
												 name = "master")

    masterActor ! CalculateSquares
 
  }
}
class LowLevelWorker extends Actor {
  def receive = {
    case Work(startAt, range, step) =>
      sender ! ResultValue(calculateSquare(startAt, range, step))
  }
  
  def calculateSquare(start: Int, range: Int, step: Int) : List[Int] = 	 {
    var tempSum: Long = 0
	var n: Int = 0
	var sqrt: Double = 0.0
	var count: Int = 0
	var list :ListBuffer[Int] = ListBuffer()
	for (i <- start until (start + step)) {
		tempSum = 0
		for (j <- i until i + range) {
			tempSum = tempSum + j*j
		}
		sqrt = Math.sqrt(tempSum)
		if (sqrt == sqrt.toInt) {
			list += i
			count = count + 1
		}
	}
	var rList:List[Int]  = list.toList
	rList
  }
}

class Master(Workers: Int, endAt: Int, range: Int, step: Int)
  extends Actor {
 
  var resultCount: Int = 0
  val workerRouter = context.actorOf(
    Props[LowLevelWorker].withRouter(RoundRobinRouter(Workers)), name = "workerRouter")
 
  def receive = {
	case CalculateSquares =>
	
		for (i <- 1 until endAt by step) {
			workerRouter ! Work(i,range,step)
		}

    case ResultValue(value) =>
		resultCount += 1
		if (value.length > 0 && value(0) != 0) {
			for(i <- 0 until value.length) {
				if (value(i) != 0)
					println(value(i))
			}
		}
		if (resultCount == (endAt/step)) {
			context.stop(self)
			context.system.shutdown()
		}
  }
 
}
