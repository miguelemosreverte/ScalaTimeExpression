import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.DAYS

object TimeExpression {

  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(localDate: LocalDate): TimeExpression = (givenlocalDate: LocalDate) => givenlocalDate == localDate


  def daily(every: Int, from: LocalDate): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = {
      val isAfterTruly = givenlocalDate.compareTo(from) >= 0
      val daysBetween = DAYS.between(from, givenlocalDate)
     return isAfterTruly && (daysBetween % every == 0)
    }
  }

  def monthlyEvery(amountOfMonth: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = ???

  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, from: YearMonth): TimeExpression = ???

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = ???

}

trait TimeExpression {

  def isRecurringOn(localDate: LocalDate): Boolean
}
