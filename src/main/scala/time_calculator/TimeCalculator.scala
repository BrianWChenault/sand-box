package time_calculator

import scala.util.Try
import scala.util.matching.Regex

object TimeCalculator {
  lazy val timeFormat = """([0-9]{1,2}):([0-9]{2})\s(?i)(AM|PM)""".r
  val minutesInPeriod = 12 * 60

  case class Time(hour: Int, minute: Int, period: String, error: ErrorHolder)
  case class Update(value: Int, error: ErrorHolder)

  def updateTime(time: Time, update: Update): Time = {
    // we shift from AM -> PM at 12, so for nice maths we will consider 12 our 0 hour
    val workingHour = if (time.hour == 12) 0  else time.hour

    val minutesPastPeriodBase = (workingHour * 60) + time.minute
    val updatedMinutesPastPeriodBase = minutesPastPeriodBase + update.value
    val minutesIntoCurrentPeriod = updatedMinutesPastPeriodBase % minutesInPeriod
    val normalizedMinutesIntoCurrentPeriod = if (minutesIntoCurrentPeriod  < 0) {
      minutesInPeriod + minutesIntoCurrentPeriod
    } else {
      minutesIntoCurrentPeriod
    }

    val updatedWorkingHour = normalizedMinutesIntoCurrentPeriod / 60
    val updatedHour = if (updatedWorkingHour == 0) 12 else updatedWorkingHour
    val updatedMinute = normalizedMinutesIntoCurrentPeriod % 60

    val updatedPeriod = calculateNewPeriod(minutesPastPeriodBase, update.value, time.period)

    Time(
      hour = updatedHour,
      minute = updatedMinute,
      period = updatedPeriod,
      error = ErrorHolder()
    )
  }

  private def calculateNewPeriod(minutesPastPeriodBase: Int, updateValue: Int, initialPeriod: String): String = {
    val distanceToFirstTransition = if (updateValue < 0) {
      minutesPastPeriodBase
    } else {
      minutesInPeriod - minutesPastPeriodBase
    }
    val normalizedUpdateDistance = Math.abs(updateValue)

    if (normalizedUpdateDistance < distanceToFirstTransition) {
      initialPeriod
    } else {
      val distancePastFirstTransition = normalizedUpdateDistance - distanceToFirstTransition
      val numberOfTransitions = 1 + (distancePastFirstTransition / minutesInPeriod)
      val isPeriodChange = numberOfTransitions % 2 == 1
      if (isPeriodChange) {
        if (initialPeriod == "AM") "PM" else "AM"
      } else {
        initialPeriod
      }
    }
  }

  def extractUpdate(rawUpdate: String): Update = {
    Try {
      val parsedValue = rawUpdate.toInt
      Update(
        value = parsedValue,
        error = ErrorHolder()
      )
    }
      .getOrElse(
        Update(
          value = 0,
          error = ErrorHolder(
            isError = true,
            errorLog = Seq(
              s"Value of update was not an Int"
            )
          )
        )
      )
  }

  def extractTime(rawTime: String): Time = {
      getExactMatch(rawTime, timeFormat) match {
        case Some(matchGroups) => {
          val hour = matchGroups.group(1).toInt
          val minute = matchGroups.group(2).toInt
          val period = matchGroups.group(3)

          // If you ever have new thing to validate, add to this list
          val error = Seq(
            validateHour(hour.toInt),
            validateMinute(minute.toInt)
          )
            .flatten
            .foldLeft(ErrorHolder())((collectedErrors, newError) => collectedErrors.conflate(newError))

          Time(hour, minute, period, error)
        }
        case None =>
          val error = ErrorHolder(
            isError = true,
            errorLog = Seq(
              "Time string format unrecognized, required format: (H)H:MM {AM|PM}"
            )
          )
          Time(0, 0, "", error)
      }
  }

  private def validateHour(hour: Int): Option[ErrorHolder] = {
    if (hour > 0 && hour < 13) {
      None
    } else {
      Some(ErrorHolder(
        isError = true,
        errorLog = Seq(
          s"Hour value out of range, $hour is not between 1 and 12"
        )
      ))
    }
  }

  private def validateMinute(minute: Int): Option[ErrorHolder] = {
    if (minute >= 0 && minute < 61) {
      None
    } else {
      Some(ErrorHolder(
        isError = true,
        errorLog = Seq(
          s"Minute value out of range, $minute is not between 0 and 60"
        )
      ))
    }
  }


  // I think I write this function every time I work with regex.  I don't think the scala regex library has a "return a match
  // if the entire string matches exactly, None otherwise."  Maybe I am missing it
  private def getExactMatch(value: String, matcher: Regex): Option[Regex.Match] = {
    if (matcher.matches(value)) {
      matcher.findFirstMatchIn(value)
    } else {
      None
    }
  }
}
