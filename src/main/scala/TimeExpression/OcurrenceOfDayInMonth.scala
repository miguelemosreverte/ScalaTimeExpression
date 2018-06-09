package TimeExpression


object OcurrenceOfDayInMonth {
  sealed trait PatternMatch
  case object First extends PatternMatch
  case object Second extends PatternMatch
  case object Third extends PatternMatch
  case object Fourth extends PatternMatch
  case object Fifth extends PatternMatch
  case object Last extends PatternMatch
}