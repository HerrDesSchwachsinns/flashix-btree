package helpers.scala

case class NumberException(msg : String) extends Exception("number exception: " + msg) { }
case class InvalidSelector(msg : String) extends Exception("invalid selector: " + msg) { }
case class InvalidSelectorUpdate(msg : String) extends Exception("invalid selector update: " + msg) { }
case class InitializationFailure() extends Exception("initialization failed") { }
case class RecoveryFailure() extends Exception("recovery failed") { }
case class ChooseFailure() extends Exception("choose failed") { }
case class DecodeFailure() extends Exception("decoding failed") { }
