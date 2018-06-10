package TimeExpression

import java.time.temporal.ChronoUnit
import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}

import Ocurrence.DayOfWeekOcurrence.{Fifth, First, Fourth, Last, Second, Third}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS, YEARS}

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
      &&  Specificity.happensTheXWeekOfTheMonth(weekOfMonth, from.atDay(1), givenlocalDate)
      &&  Specificity.specifically(dayOfWeek, givenlocalDate.getDayOfWeek))



  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Ocurrence.DayOfWeekOcurrence.PatternMatch, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = {

      val monthsBetweenFollowsTheRule = Recurrence.happensEveryXMonths(amountMonth, from.atDay(1), givenlocalDate)
      val dayOfWeekFollowsTheRule =  weekOfMonth match {
        case First =>   Specificity happensTheXWeekOfTheMonth(1, from.atDay(1), givenlocalDate)
        case Second =>  Specificity.happensTheXWeekOfTheMonth(2, from.atDay(1), givenlocalDate)
        case Last =>    Specificity.happensTheXWeekOfTheMonth(
          Ocurrence.DayOfWeekOcurrence.inMonth(dayOfWeek, YearMonth.from(givenlocalDate)),
          from.atDay(1),
          givenlocalDate)

      }
      return monthsBetweenFollowsTheRule && dayOfWeekFollowsTheRule
    }
  }


  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = (givenlocalDate: LocalDate) =>
          Specificity.specifically(day, MonthDay.from(givenlocalDate))

  def monthlyEvery(dayOfWeek: DayOfWeek, weekOfMonth: Ocurrence.DayOfWeekOcurrence.PatternMatch, from: LocalDate, givenlocalDate:LocalDate): Boolean = weekOfMonth match {
        case First =>   Specificity.happensTheXWeekOfTheMonth(dayOfWeek.getValue, from, givenlocalDate)
        case Second =>  Specificity.happensTheXWeekOfTheMonth(dayOfWeek.getValue, from, givenlocalDate)
        case Third =>   Specificity.happensTheXWeekOfTheMonth(dayOfWeek.getValue, from, givenlocalDate)
        case Fourth =>  Specificity.happensTheXWeekOfTheMonth(dayOfWeek.getValue, from, givenlocalDate)
        case Fifth =>   Specificity.happensTheXWeekOfTheMonth(dayOfWeek.getValue, from, givenlocalDate)
        case Last =>    Specificity.happensTheXWeekOfTheMonth(Ocurrence.DayOfWeekOcurrence.inMonth(dayOfWeek, YearMonth.from(givenlocalDate)), from, givenlocalDate)
        }






  object SpecificityWithDefaults {
    implicit def SpecificityToSpecificityWithDefaults(o: Specificity.type): Object {

      def happensAtXPeriodOfTheMonth(period: ChronoUnit, periodIndex: Int, from: LocalDate, givenLocalDate: LocalDate): Boolean
      def happensTheXWeekOfTheMonth(everyXWeek: Int, from: LocalDate, givenlocalDate: LocalDate): Boolean
    
    } = new {

      def happensAtXPeriodOfTheMonth(period : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate)
      : Boolean = periodIndex match {
        case -1 => Specificity.happensAtTheLastPeriodOfTheYPeriod(period, periodIndex, from, givenLocalDate, MONTHS)
        case _ => Specificity.happensAtTheXPeriodOfTheYPeriod(period, periodIndex, from, givenLocalDate, MONTHS)
      }



      def happensTheXWeekOfTheMonth(everyXWeek: Int,
                                    from: LocalDate,
                                    givenlocalDate: LocalDate)
      : Boolean = happensAtXPeriodOfTheMonth(WEEKS, everyXWeek, from, givenlocalDate)
    }
  }



}