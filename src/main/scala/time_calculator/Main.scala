package time_calculator

object Main  extends App {

  if (args.length == 2) {
    runCalculator(args(0), args(1))
  } else {
    println("Invalid Arguments: \n - must specify a base time and an update \nExample: run \"9:13 AM\" 200")
  }

  private def runCalculator(rawTime: String, rawUpdate: String): Unit = {
    val time = TimeCalculator.extractTime(rawTime)
    val update = TimeCalculator.extractUpdate(rawUpdate)

    if (!time.error.isError && !update.error.isError) {
      val updatedTime = TimeCalculator.updateTime(time, update)

      println(updatedTime.hour + ":" + updatedTime.minute + " " + updatedTime.period)
    } else {
      val allErrors = time.error.conflate(update.error)

      println("Invalid Arguments: ")
      allErrors.errorLog.foreach(error => println(s" - $error"))
    }
  }
}

