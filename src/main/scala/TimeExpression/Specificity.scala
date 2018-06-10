package TimeExpression

import java.time.temporal.ChronoUnit.{MONTHS, WEEKS, YEARS}
import java.time.{DayOfWeek, LocalDate, MonthDay}

object Specificity {

  def specifically(thisDate : Int, thisOtherDate : Int): Boolean = thisDate == thisOtherDate
  def specifically(thisDate : MonthDay, thisOtherDate : MonthDay): Boolean = thisDate == thisOtherDate
  def specifically(thisDate : DayOfWeek, thisOtherDate : DayOfWeek): Boolean = thisDate == thisOtherDate


  sealed abstract class SupportedChronoUnits
  case object Week extends SupportedChronoUnits
  case object Month extends SupportedChronoUnits
  case object Year extends SupportedChronoUnits

  def periodsBetween(periodX : java.time.temporal.ChronoUnit, givenLocalDate : LocalDate, periodY : Option[SupportedChronoUnits])
 : Option[Int] = {
    val deltaDays : Option[Int] = periodY match  {
      case Some(Week) => Some(givenLocalDate.getDayOfWeek.getValue)
      case Some(Month) => Some(givenLocalDate.getDayOfMonth)
      case Some(Year) => Some(givenLocalDate.getDayOfYear)
      case None => None
    }
    deltaDays match {
      case Some(value) => {
        val from = givenLocalDate.minusDays(value)
        Some(periodX.between(from, givenLocalDate).toInt)
      }
      case None => None
    }
  }


  def happensAtTheXPeriodOfTheYPeriod(periodX : java.time.temporal.ChronoUnit, periodIndex : Int, from: LocalDate, givenLocalDate : LocalDate, periodY : java.time.temporal.ChronoUnit)
    : Option[Boolean] =
    periodsBetween(periodX, givenLocalDate, periodY) match {
      case Some(value) => Some(periodIndex == value)
      case None => None
    }


  //TODO see if I can call this function implicitely
  implicit def convertToSupportedChronoUnits(chronoUnit : java.time.temporal.ChronoUnit)
  : Option[SupportedChronoUnits] =
    chronoUnit.toString match {
      case "Weeks" => Some(Week)
      case "Months" => Some(Month)
      case "Years" => Some(Year)
      case _ => None
    }


}

