#' @rdname DataTypeChecks
#'
#' @name DataTypeChecks
#'
#' @title Data Type checks for valid string representations of R data types
#'
#' @description Identify valid R data types from strings for proper type casting during data analysis and munging
#'
#' @details This functions assist in many generic tasks such as configuration type casting, branching logic syntax conversion and data type checking during error reporting.
#'
#' They are simply regular expressions that are evaluated to boolean outputs depending on whether the predicate meets the requirement.
#'
NULL

#'
#' @rdname DataTypeChecks
#' @param x a character vector of unit length
#' @export

is_date = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(regexpr("^[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}$", str_trim(x)) > 0L)
}

#'
#' @rdname DataTypeChecks
#' @export

is_int = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(regexpr("^[\\-]?[0-9]*$", str_trim(x)) > 0L)
}

#'
#' @rdname DataTypeChecks
#' @export

is_number = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(regexpr("^[\\-]?[0-9]*[\\.]?[0-9]*$", str_trim(x)) > 0L)
}

#'
#' @rdname DataTypeChecks
#' @export

is_boolean = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(x %in% c("T", "F", "TRUE", "FALSE"))
}
