import org.scalatest._
import time_calculator.{ErrorHolder, TimeCalculator}
import time_calculator.TimeCalculator._

class TimeCalculatorSpec extends FlatSpec with Matchers {
  "Time Calculator" should "extract input time" in {
    val rawTime = "4:17 PM"
    val extractedTime = TimeCalculator.extractTime(rawTime)

    extractedTime.hour shouldBe 4
    extractedTime.minute shouldBe 17
    extractedTime.period shouldBe "PM"
    extractedTime.error.isError shouldBe false
  }

  it should "collect errors from incorrectly formatted time" in {
    val rawTime = "18:77 AM"
    val extractedTime = TimeCalculator.extractTime(rawTime)

    extractedTime.error.isError shouldBe true
    extractedTime.error.errorLog should contain theSameElementsAs Seq(
      "Hour value out of range, 18 is not between 1 and 12",
      "Minute value out of range, 77 is not between 0 and 60"
    )
  }

  it should "increment a variety of times correctly" in {
    val time_0 = Time(2, 30, "PM", ErrorHolder())
    val update_0 = Update(60, ErrorHolder())

    val updatedTime_0 = TimeCalculator.updateTime(time_0, update_0)
    updatedTime_0.hour shouldBe 3
    updatedTime_0.minute shouldBe 30
    updatedTime_0.period shouldBe "PM"

    val time_1 = Time(10, 30, "PM", ErrorHolder())
    val update_1 = Update(120, ErrorHolder())

    val updatedTime_1 = TimeCalculator.updateTime(time_1, update_1)
    updatedTime_1.hour shouldBe 12
    updatedTime_1.minute shouldBe 30
    updatedTime_1.period shouldBe "AM"


    val time_2 = Time(1, 30, "PM", ErrorHolder())
    val update_2 = Update(-139, ErrorHolder())

    val updatedTime_2 = TimeCalculator.updateTime(time_2, update_2)
    updatedTime_2.hour shouldBe 11
    updatedTime_2.minute shouldBe 11
    updatedTime_2.period shouldBe "AM"


    val time_3 = Time(1, 30, "PM", ErrorHolder())
    val update_3 = Update(-5760, ErrorHolder())

    val updatedTime_3 = TimeCalculator.updateTime(time_3, update_3)
    updatedTime_3.hour shouldBe 1
    updatedTime_3.minute shouldBe 30
    updatedTime_3.period shouldBe "PM"

    val time_4 = Time(11, 59, "PM", ErrorHolder())
    val update_4 = Update(3, ErrorHolder())

    val updatedTime_4 = TimeCalculator.updateTime(time_4, update_4)
    updatedTime_4.hour shouldBe 12
    updatedTime_4.minute shouldBe 2
    updatedTime_4.period shouldBe "AM"
  }
}
