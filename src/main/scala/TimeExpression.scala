import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.{DAYS, WEEKS, MONTHS, YEARS}

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
      val daysBetween = DAYS.between(from, givenlocalDate)
      val daysBetweenFollowsTheRule = daysBetween % every == 0
     return daysBetweenFollowsTheRule
    }
  }

  def monthlyEvery(amountOfMonth: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = {
      val monthsBetweenFollowsTheRule = MONTHS.between(from, givenlocalDate) % amountOfMonth == 0
      val dayOfMonthFollowsTheRule = dayOfMonth == givenlocalDate.getDayOfMonth
      return monthsBetweenFollowsTheRule && dayOfMonthFollowsTheRule
    }
  }

  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = {
      val monthsBetweenFollowsTheRule = MONTHS.between(from, givenlocalDate) % amountMonth == 0
      val firstDayOfMonth = givenlocalDate.minusDays(givenlocalDate.getDayOfMonth)
      val givenLocalDateGetWeekOfMonth =  WEEKS.between(firstDayOfMonth, givenlocalDate)
      val weeksBetweenFollowsTheRule = givenLocalDateGetWeekOfMonth == weekOfMonth

      val dayOfWeekFollowsTheRule = dayOfWeek == givenlocalDate.getDayOfWeek
      println("monthly every B")
      println(monthsBetweenFollowsTheRule, from, givenlocalDate)
      println(weeksBetweenFollowsTheRule, from.atDay(1), givenlocalDate, givenlocalDate.getDayOfMonth, givenLocalDateGetWeekOfMonth, weekOfMonth)
      println(dayOfWeekFollowsTheRule, dayOfWeek, givenlocalDate.getDayOfWeek)

      println()
      println("Why is weekOfMonth 3??? weekOfMonth:" + weekOfMonth)
      println(amountMonth, dayOfWeek, weekOfMonth, from)
      println(givenlocalDate, givenLocalDateGetWeekOfMonth)
      return monthsBetweenFollowsTheRule && weeksBetweenFollowsTheRule && dayOfWeekFollowsTheRule
    }
  }

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression
      = (givenlocalDate: LocalDate) => day.getDayOfMonth == givenlocalDate.getDayOfMonth



}

trait TimeExpression {

  def isRecurringOn(localDate: LocalDate): Boolean
}
