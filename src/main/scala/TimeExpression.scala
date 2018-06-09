import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}

object TimeExpression {

  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(localDate: LocalDate): TimeExpression = ???


  def daily(every: Int, from: LocalDate): TimeExpression = ???

  def monthlyEvery(amountOfMonth: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = ???

  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, from: YearMonth): TimeExpression = ???

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = ???

}

trait TimeExpression {

  def isRecurringOn(localDate: LocalDate): Boolean
}
