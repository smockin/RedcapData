

#' @rdname DataValidation
#'
#' @include data_types.R
#'
#' @name date_can_be_validated
#'
#' @title Check whether a date can be used for validation
#'
#' @description Check whether a date can be used for validation
#'
#' @details This performs checks to see if a date can be used for validation in error reporting.
#'
#' This prevents unnecessary errors and validations resulting from invalid or default data inputs.
#'
#' @param var The variable to check
#'
#' @export
#'

date_can_be_validated = function(var) {
  if (is.na(var))
    return(F)
  if (is.null(var))
    return(F)
  if (str_trim(var) == "")
    return(F)
  var = str_trim(var)
  if (!is_date(var))
    return(F)
  if (as.Date(var) %in% seq(as.Date("1910-01-01"), as.Date("1950-01-01"), by = "year"))
    return(F)
  T
}

#' @rdname DataValidation
#'
#' @name data_can_be_validated
#'
#' @export
#'

data_can_be_validated = function(var) {
  if (is.null(var))
    return(F)
  if (is.na(var))
    return(F)
  if (str_trim(tolower(as.character(var))) %in% c("", "-1", "empty"))
    return(F)
  var = str_trim(var)
  if (is_date(var))
    return(date_can_be_validated(var))
  T
}

#' @rdname DataValidation
#'
#' @name data_missing
#'
#' @export
#'

data_missing = function(var) {
  if (is.null(var))
    return(T)
  if (is.na(var))
    return(T)
  if (stringr::str_trim(var) == "")
    return(T)
  F
}
