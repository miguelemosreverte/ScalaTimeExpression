package TimeExpression

import java.time.temporal.ChronoUnit.{MONTHS, WEEKS}
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}

trait TimeExpression {

  def isRecurringOn(localDate: LocalDate): Boolean
}

object TimeExpression {



  import SpecificityWithDefaults.SpecificityToSpecificityWithDefaults

  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(localDate: LocalDate): TimeExpression = (givenlocalDate: LocalDate) =>
          Recurrence.NonReccurrent(localDate, givenlocalDate)

  def daily(every: Int, from: LocalDate): TimeExpression = (givenlocalDate: LocalDate) =>
          Recurrence.happensEveryXDays(every, from, givenlocalDate)

  def monthlyEvery(amountOfMonth: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = (givenlocalDate: LocalDate) => (
          Recurrence.happensEveryXMonths(amountOfMonth, from.atDay(1), givenlocalDate)
      &&  Specificity.specifically(dayOfMonth, givenlocalDate.getDayOfMonth))

  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, from: YearMonth): TimeExpression = (givenlocalDate: LocalDate) => (
          Recurrence.happensEveryXMonths(amountMonth, from.atDay(1), givenlocalDate)
      &&  Specificity.happensTheXWeekOfTheMonth(weekOfMonth, dayOfWeek, from.atDay(1), givenlocalDate)
      &&  Specificity.specifically(dayOfWeek, givenlocalDate.getDayOfWeek))

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = (givenlocalDate: LocalDate) =>
          Specificity.specifically(day, MonthDay.from(givenlocalDate))





  object SpecificityWithDefaults {
    implicit def SpecificityToSpecificityWithDefaults(o: Specificity.type): Object {
      def happensAtXPeriodOfTheMonth(period: ChronoUnit, periodIndex: Int, from: LocalDate, givenLocalDate: LocalDate): Boolean

      def happensAtXPeriodOfTheMonth(period: ChronoUnit, dayOfWeek: DayOfWeek, from: LocalDate, givenLocalDate: LocalDate): Boolean

      def happensTheXWeekOfTheMonth(everyXWeek: Int, dayOfWeek: DayOfWeek, from: LocalDate, givenlocalDate: LocalDate): Boolean
    } = new {



      def happensTheXWeekOfTheMonth(weekIndex: Int,
                                    dayOfWeek: DayOfWeek,
                                    from: LocalDate,
                                    givenlocalDate: LocalDate)
      : Boolean = weekIndex match {
        case -1 => happensAtXPeriodOfTheMonth(WEEKS, dayOfWeek, from, givenlocalDate)
        case _ => happensAtXPeriodOfTheMonth(WEEKS, weekIndex, from, givenlocalDate)
      }


      def happensAtXPeriodOfTheMonth(period : java.time.temporal.ChronoUnit, dayOfWeek : DayOfWeek, from: LocalDate, givenLocalDate : LocalDate)
      : Boolean = {
        def lastOcurrenceOfDayOfWeekInMonth(dow: DayOfWeek, month: YearMonth) : Int = {
          val start = month.atDay(1).`with`(TemporalAdjusters.nextOrSame(dow))
          val end = month.atEndOfMonth()
          return ChronoUnit.WEEKS.between(start, end).toInt
        }
        val inferredLastPeriodIndex = lastOcurrenceOfDayOfWeekInMonth(dayOfWeek, YearMonth.from(givenLocalDate))
        Specificity.happensAtTheXPeriodOfTheYPeriod(period, inferredLastPeriodIndex, from, givenLocalDate, MONTHS).getOrElse(false)
      }

      def happensAtXPeriodOfTheMonth(period : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate)
      : Boolean = Specificity.happensAtTheXPeriodOfTheYPeriod(period, periodIndex, from, givenLocalDate, MONTHS).getOrElse(false)



    }
  }



}