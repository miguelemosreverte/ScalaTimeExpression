package TimeExpression

import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS}
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import OcurrenceOfDayInMonth.{First, Second, Last}

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



  def daily(every: Int, from: LocalDate): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = Recurrence.happensEveryXDays(every, from, givenlocalDate)
  }

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



  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: OcurrenceOfDayInMonth.PatternMatch, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = {

      val monthsBetweenFollowsTheRule = Recurrence.happensEveryXMonths(amountMonth, from.atDay(1), givenlocalDate)
      val firstDayOfMonth = givenlocalDate.minusDays(givenlocalDate.getDayOfMonth-1)
      val givenLocalDateGetWeekOfMonth =  WEEKS.between(firstDayOfMonth, givenlocalDate)

      def countDayOccurenceInMonth(dow: DayOfWeek, month: YearMonth) : Long = {
        val start = month.atDay(1).`with`(TemporalAdjusters.nextOrSame(dow))
        return ChronoUnit.WEEKS.between(start, month.atEndOfMonth())
      }

      val dayOfWeekFollowsTheRule =  weekOfMonth match {
        case First =>  givenLocalDateGetWeekOfMonth == 1 //primitive obsession, maybe. I think I could improve this? (case class)
        case Second => givenLocalDateGetWeekOfMonth == 2 //I refuse to use case classes because I find case objects so cool!
        case Last =>   givenLocalDateGetWeekOfMonth == countDayOccurenceInMonth(dayOfWeek, YearMonth.from(givenlocalDate))


      }
      return monthsBetweenFollowsTheRule && dayOfWeekFollowsTheRule
    }
  }


  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression
      = (givenlocalDate: LocalDate) => Specificity.specifically(day, MonthDay.from(givenlocalDate))



}