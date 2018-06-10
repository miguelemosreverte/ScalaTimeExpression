package TimeExpression

import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS}
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import Ocurrence.DayOfWeekOcurrence.{First, Second, Last}

trait TimeExpression {

  def isRecurringOn(localDate: LocalDate): Boolean
}

object TimeExpression {


  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(localDate: LocalDate): TimeExpression = (givenlocalDate: LocalDate) => givenlocalDate == localDate



  def daily(every: Int, from: LocalDate): TimeExpression = (givenlocalDate: LocalDate) => Recurrence.happensEveryXDays(every, from, givenlocalDate)

  def monthlyEvery(amountOfMonth: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = (
            Recurrence.happensEveryXMonths(amountOfMonth, from.atDay(1), givenlocalDate)
        &&  Specificity.specifically(dayOfMonth, givenlocalDate.getDayOfMonth))

  }



  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = (
            Recurrence.happensEveryXMonths(amountMonth, from.atDay(1), givenlocalDate)
        &&  Specificity.happensTheXWeekOfTheMonth(weekOfMonth, from.atDay(1), givenlocalDate)
        &&  Specificity.specifically(dayOfWeek, givenlocalDate.getDayOfWeek))
  }



  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Ocurrence.DayOfWeekOcurrence.PatternMatch, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = {

      val monthsBetweenFollowsTheRule = Recurrence.happensEveryXMonths(amountMonth, from.atDay(1), givenlocalDate)



      val dayOfWeekFollowsTheRule =  weekOfMonth match {
        case First =>   Specificity.happensTheXWeekOfTheMonth(1, from.atDay(1), givenlocalDate)
        case Second =>  Specificity.happensTheXWeekOfTheMonth(2, from.atDay(1), givenlocalDate)
        case Last =>    Specificity.happensTheXWeekOfTheMonth(Ocurrence.DayOfWeekOcurrence.inMonth(dayOfWeek, YearMonth.from(givenlocalDate)), from.atDay(1), givenlocalDate)


      }
      return monthsBetweenFollowsTheRule && dayOfWeekFollowsTheRule
    }
  }


  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression
      = (givenlocalDate: LocalDate) => Specificity.specifically(day, MonthDay.from(givenlocalDate))



}