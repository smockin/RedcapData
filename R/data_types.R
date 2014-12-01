is_date = function(x) {
  x = as.character(x)
  isTRUE(regexpr("^[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}$", str_trim(x)) > 0L)
}

is_int = function(x) {
  x = as.character(x)
  isTRUE(regexpr("^[\\-]?[0-9]*$", str_trim(x)) > 0L)
}

is_number = function(x) {
  x = as.character(x)
  isTRUE(regexpr("^[\\-]?[0-9]*[\\.]?[0-9]*$", str_trim(x)) > 0L)
}
