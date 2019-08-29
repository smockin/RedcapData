

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
  isTRUE(regexpr(
    "^[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}$", str_trim(x)
  ) > 0L)
}

#'
#' @rdname DataTypeChecks
#' @export

is_int = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  . = isTRUE(regexpr("^[\\-]?[0-9]*$", str_trim(x)) > 0L)
  if (!.)
    . = isTRUE(regexpr('^[\\-]?[0-9]*[\\.]?[0-9]?e\\+[0-9]*$', str_trim(x)) > 0L)
  .
}

#'
#' @rdname DataTypeChecks
#' @export

is_number = function(x) {
  x = as.character(x)
  if (is.na(x) | str_trim(x) == "")
    return(TRUE)
  if (is_int(x))
    return(TRUE)
  . = isTRUE(regexpr("^[\\-]?[0-9]*[\\.]?[0-9]*$", str_trim(x)) > 0L)
  if (!.)
    . = isTRUE(regexpr('^[\\-]?[0-9]*[\\.]?[0-9]?e\\+[0-9]*$', str_trim(x)) > 0L)
  .
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


#'
#' @rdname DataTypeChecks
#' @export

is_checkbox = function(varName, metadataName) {
  meta=try(as.data.frame(get(metadataName)), silent=T)
  if(class(meta)=="try-error"){
    meta=data.frame(metadataName)
  }
  setDT(meta)
  if(!(all(c("field_name", 'field_type') %in% names(meta)))){
    stop("Metadata must have `field_name` and `field_type`")
  }
  varName = as.character(varName)
  if(
    isTRUE(
      meta[field_name==varName, field_type=='checkbox']
    )){
    return(T)
  }else{
    F
  }
}
