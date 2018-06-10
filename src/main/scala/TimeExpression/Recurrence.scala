package TimeExpression

import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS, YEARS}

object Recurrence {



  def NonReccurrent(localDate: LocalDate): TimeExpression = (givenlocalDate: LocalDate) => givenlocalDate == localDate

  def happensEveryX(period : java.time.temporal.ChronoUnit, every : Int, from: LocalDate, givenLocalDate : LocalDate)
  : Boolean = period.between(from, givenLocalDate) % every == 0

  def happensEveryXMonths(everyXMonths: Int,
                          from: LocalDate,
                          givenlocalDate: LocalDate)
  : Boolean = happensEveryX(MONTHS, everyXMonths, from, givenlocalDate)

  def happensEveryXDays(everyXDays: Int,
                          from: LocalDate,
                          givenlocalDate: LocalDate)
  : Boolean = happensEveryX(DAYS, everyXDays, from, givenlocalDate)



  def happensAtTheXPeriodOfTheYPeriod(periodX : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate, periodY : java.time.temporal.ChronoUnit)
    : Boolean = {
    val deltaDays : Int = periodY match  {
      case WEEKS => givenLocalDate.getDayOfWeek.getValue
      case MONTHS => givenLocalDate.getDayOfMonth
      case YEARS => givenLocalDate.getDayOfYear
      }
    val from = givenLocalDate.minusDays(deltaDays)
    return periodX.between(from, givenLocalDate) ==  periodIndex
  }

  def happensAtXPeriodOfTheMonth(period : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate)
  : Boolean = happensAtTheXPeriodOfTheYPeriod(WEEKS, periodIndex, from, givenLocalDate, MONTHS)

  def happensTheXWeekOfTheMonth(everyXWeek: Int,
                        from: LocalDate,
                        givenlocalDate: LocalDate)
  : Boolean = happensAtXPeriodOfTheMonth(WEEKS, everyXWeek, from, givenlocalDate)



  def specifically(thisDate : MonthDay, thisOtherDate : MonthDay): Boolean = thisDate == thisOtherDate
  def specifically(thisDate : DayOfWeek, thisOtherDate : DayOfWeek): Boolean = thisDate == thisOtherDate
}
