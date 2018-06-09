import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS}
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import OcurrenceOfDayInMonth.{First, Second, Last}


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



}
