package TimeExpression

import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS, YEARS}
import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}

object Specificity {

  def specifically(thisDate : Int, thisOtherDate : Int): Boolean = thisDate == thisOtherDate
  def specifically(thisDate : MonthDay, thisOtherDate : MonthDay): Boolean = thisDate == thisOtherDate
  def specifically(thisDate : DayOfWeek, thisOtherDate : DayOfWeek): Boolean = thisDate == thisOtherDate



  def periodsBetween(periodX : java.time.temporal.ChronoUnit, givenLocalDate : LocalDate, periodY : java.time.temporal.ChronoUnit)
 : Int = {
    val deltaDays : Int = periodY match  {
      case WEEKS => givenLocalDate.getDayOfWeek.getValue
      case MONTHS => givenLocalDate.getDayOfMonth
      case YEARS => givenLocalDate.getDayOfYear
    }
    val from = givenLocalDate.minusDays(deltaDays)
    return periodX.between(from, givenLocalDate).toInt
  }


  def happensAtTheXPeriodOfTheYPeriod(periodX : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate, periodY : java.time.temporal.ChronoUnit)
    : Boolean = periodsBetween(periodX, givenLocalDate, periodY) ==  periodIndex

  def happensAtTheLastPeriodOfTheYPeriod(periodX : java.time.temporal.ChronoUnit,
                                         periodIndex : Int,
                                         from: LocalDate,
                                         givenLocalDate : LocalDate,
                                         periodY : java.time.temporal.ChronoUnit)
  : Boolean = periodsBetween(periodX, givenLocalDate, periodY) ==  periodIndex



  def happensAtXPeriodOfTheMonth(period : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate)
  : Boolean = periodIndex match {
    case -1 => happensAtTheLastPeriodOfTheYPeriod(period, periodIndex, from, givenLocalDate, MONTHS)
    case _ => happensAtTheXPeriodOfTheYPeriod(period, periodIndex, from, givenLocalDate, MONTHS)
  }

  def happensAtXPeriodOfTheYear(period : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate)
  : Boolean = periodIndex match {
    case -1 => happensAtTheLastPeriodOfTheYPeriod(period, periodIndex, from, givenLocalDate, YEARS)
    case _ => happensAtTheXPeriodOfTheYPeriod(period, periodIndex, from, givenLocalDate, YEARS)
  }



  def happensTheXWeekOfTheMonth(everyXWeek: Int,
                        from: LocalDate,
                        givenlocalDate: LocalDate)
  : Boolean = happensAtXPeriodOfTheMonth(WEEKS, everyXWeek, from, givenlocalDate)



  def happensTheXWeekOfTheYear(everyXWeek: Int,
                                from: LocalDate,
                                givenlocalDate: LocalDate)
  : Boolean = happensAtXPeriodOfTheYear(WEEKS, everyXWeek, from, givenlocalDate)

  def happensTheXMonthOfTheYear(everyXWeek: Int,
                                from: LocalDate,
                                givenlocalDate: LocalDate)
  : Boolean = happensAtXPeriodOfTheYear(MONTHS, everyXWeek, from, givenlocalDate)



}
