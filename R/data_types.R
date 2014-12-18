#' @name DataType_Checks
#'
#' @title Data Type checks for valid string representations of R data types
#'
#' @description Identify valid data tpes from strings for proper casting
#'
#' @details This functions assist in many generic tasks such as configuration type casting and data type checking in error reports.
NULL

#'
#' @rdname DataType_Checks
#' @param x a character vector of length one

is_date = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(regexpr("^[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}$", str_trim(x)) > 0L)
}

#'
#' @rdname DataType_Checks
#' @param x a character vector of length one

is_int = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(regexpr("^[\\-]?[0-9]*$", str_trim(x)) > 0L)
}

#'
#' @rdname DataType_Checks
#' @param x a character vector of length one

is_number = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(regexpr("^[\\-]?[0-9]*[\\.]?[0-9]*$", str_trim(x)) > 0L)
}

#'
#' @rdname DataType_Checks
#' @param x a character vector of length one

is_boolean = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  isTRUE(x %in% c("T", "F", "TRUE", "FALSE"))
}
