package time_calculator

case class ErrorHolder(isError: Boolean = false, errorLog: Seq[String] = Seq.empty) {
  def conflate(right: ErrorHolder): ErrorHolder = {
    ErrorHolder(
      isError = isError || right.isError,
      errorLog = errorLog ++ right.errorLog
    )
  }
}

