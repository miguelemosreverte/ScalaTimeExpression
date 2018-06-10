package TimeExpression

import java.time.{DayOfWeek, LocalDate, YearMonth}
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS, YEARS}


object Ocurrence {

  object DayOfWeekOcurrence {
    sealed trait PatternMatch
    case object First extends PatternMatch
    case object Second extends PatternMatch
    case object Third extends PatternMatch
    case object Fourth extends PatternMatch
    case object Fifth extends PatternMatch
    case object Last extends PatternMatch



    def inMonth(dow: DayOfWeek, month: YearMonth) : Int = {
      val start = month.atDay(1).`with`(TemporalAdjusters.nextOrSame(dow))
      val end = month.atEndOfMonth()
      return ChronoUnit.WEEKS.between(start, end).toInt
    }

    def inYear(dow: DayOfWeek, month: YearMonth) : Int = {
      val start = month.withMonth(1).atDay(1).`with`(TemporalAdjusters.nextOrSame(dow))
      val end = month.withMonth(12)
      return ChronoUnit.WEEKS.between(start, end).toInt
    }

  }


}