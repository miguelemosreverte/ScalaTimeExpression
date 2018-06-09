import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS}
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import WeekOfMonth.{First, Second, Last}

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

  def NonRecurring(localDate: LocalDate): TimeExpression = (givenlocalDate: LocalDate) => givenlocalDate == localDate

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
      return monthsBetweenFollowsTheRule && weeksBetweenFollowsTheRule && dayOfWeekFollowsTheRule
    }
  }



  def monthlyEvery(amountMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: WeekOfMonth.PatternMatch, from: YearMonth): TimeExpression = new TimeExpression {
    override def isRecurringOn(givenlocalDate: LocalDate): Boolean = {

      val monthsBetweenFollowsTheRule = MONTHS.between(from, givenlocalDate) % amountMonth == 0
      val firstDayOfMonth = givenlocalDate.minusDays(givenlocalDate.getDayOfMonth-1)
      val givenLocalDateGetWeekOfMonth =  WEEKS.between(firstDayOfMonth, givenlocalDate) + 1

      def countDayOccurenceInMonth(dow: DayOfWeek, month: YearMonth) : Long = {
        val start = month.atDay(1).`with`(TemporalAdjusters.nextOrSame(dow))
        return ChronoUnit.WEEKS.between(start, month.atEndOfMonth()) + 1
      }

      val dayOfWeekFollowsTheRule =  weekOfMonth match {
        case First =>  givenLocalDateGetWeekOfMonth == 1 //primitive obsession, maybe. I think I could improve this?
        case Second => givenLocalDateGetWeekOfMonth == 2
        case Last =>   givenLocalDateGetWeekOfMonth == countDayOccurenceInMonth(dayOfWeek, YearMonth.from(givenlocalDate))


      }
      return monthsBetweenFollowsTheRule && dayOfWeekFollowsTheRule
    }
  }


  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression
      = (givenlocalDate: LocalDate) => day.getDayOfMonth == givenlocalDate.getDayOfMonth



}

object WeekOfMonth {
  sealed trait PatternMatch
  case object First extends PatternMatch
  case object Second extends PatternMatch
  case object Last extends PatternMatch
}
