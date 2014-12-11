#' @name remove_html_tags
#'
#' @title Remove html tags from string
#'
#' @description Removes html from string which can be used as html-free text.
#'
#' @details This is a utility function for refining text.
#'
#' This is useful in cases such as autogeneration of code.
#'
#' @param x string to refine
#'
#' @export
#'
#' @return Code that can be evaluated to format data
#'
#' @include script.R
#'

remove_html_tags = function(x) {
  .namesX = names(x)
  value = sapply(x, function(s) { gsub("<[^>]*>", "", s) })
  names(value) = NULL
  value
}

#' @name logical_xpressions_red2r
#'
#' @title Reshape REDCap logic to R logical code
#'
#' @description Convert REDCap code to R syntax.
#'
#' @details For code generation, code has to be translated from one DSL or syntax to another.
#'
#' In this case, conversion from REDCap logic to the appropriate R syntax.
#'
#' This function helps map logical expressions from REDCap to R
#'
#' @param x string with REDCap logic
#'
#' @return R code that can be evaluated after further modifications
#'
#'@include data_types.R

logical_xpressions_red2r = function(x) {
  pattern_match = regexpr("[ \t]*((==)|(!=))[ \t]*'[a-zA-Z0-9]*'", x)
  if (pattern_match > 0) {
    len_match = attr(pattern_match, "match.length") + pattern_match
    start_part = substr(x, 1, pattern_match - 1)
    var_part = gregexpr("[ \t]*[a-zA-Z0-9_]+[ \t]*", start_part)[[1]]
    len_part = attr(var_part, "match.length")
    len_part = len_part[length(len_part)]
    var_part = var_part[length(var_part)]
    var_part = str_trim(substr(start_part, var_part, len_part + var_part + 1))
    match_part = substr(x, pattern_match, len_match - 1)
    end_part = substr(x, len_match, nchar(x))
    has_value = regexpr("'[a-zA-Z0-9]*'", match_part)
    has_value_len = attr(has_value, "match.length")
    value_2_chk = substr(match_part, has_value + 1, (has_value + has_value_len - 2))
    if (has_value_len > 2) {
      if (is_number(value_2_chk))
        match_part = gsub("'", "", match_part) else
          match_part = gsub("'", "\"", match_part)
    }
    else {
      test_eq = grepl("==", match_part)
      if (test_eq) {
        match_part = paste0("(any(is.na(", var_part, "), str_trim(", var_part, ") == \"\"))")
      }
      else {
        match_part = paste0("(all(!is.na(", var_part, "), str_trim(", var_part, ") != \"\"))")
      }
      start_part = substr(start_part, 1L, nchar(start_part) - len_part)
    }
    x = paste0(start_part, match_part, end_part)
    if (regexpr("[ \t]*((==)|(!=))[ \t]*'[a-zA-Z0-9]*'", x) > 0L)
      x = logical_xpressions_red2r(x)
    x
  }
  x
}

#' @name convert_missing_red2r
#'
#' @title Reshape REDCap missingness logic to appropriate R code
#'
#' @description Convert REDCap missing code to corresponding R syntax.
#'
#' @details For code generation, code has to be translated from one DSL or syntax to another.
#'
#' In this case, conversion from REDCap logic to the appropriate R syntax.
#'
#' This function helps map logical expressions from REDCap to R
#'
#' @param x string with REDCap logic
#'
#' @return R code that can be evaluated after further modifications
#'
#'@include data_types.R
#'

convert_missing_red2r = function(x) {
  reshape_na_red2r = function(x) {
    ._i = as.integer(regexpr("[a-z]", tolower(x)))
    start_part = substr(x, 1, pattern_start_pos - 1)
    name_part = substr(x, ._i, nchar(x))
    ._i = as.integer(regexpr("[^a-zA-Z0-9_]", name_part) - 1)
    name_part = substr(name_part, 1, ._i)
    if ((regexpr("!=", name_part)) > 0L) {
      cmd = paste("(!any(is.na(", name_part , "),", name_part , ' == ""))')
    }
    else {
      cmd = paste0("(any(is.na(", name_part ,"),", name_part , ' == ""))')
    }
    paste0(start_part, cmd)
  }
  string_xtract = strsplit(x, "'")[[1]]
  idx = 1:length(string_xtract)
  idx = idx[idx %% 2 == 0]
  string_xtract_chkd = string_xtract[idx]
  string_xtract_chkd = sapply(string_xtract_chkd, function(x) {
    if (x == "") {
      value = NA
    }
    else {
      value = suppressWarnings(as.numeric(x))
      if (is.na(value)) {
        value = paste('"', x, '"', sep = "")
      }
      else {
        value = x
      }
    }
    value
  })
  if (isTRUE(any(is.na(string_xtract_chkd)))) {
    idx_change = which(is.na(string_xtract_chkd))
    idx_change = (2L * idx_change) - 1L
    string_xtract[idx_change] = sapply(string_xtract[idx_change], reshape_na_red2r)
  }
  string_xtract[idx] = string_xtract_chkd
  if (isTRUE(any(is.na(string_xtract)))) {
    string_xtract = string_xtract[-which(is.na(string_xtract))]
  }
  string_xtract = paste0(string_xtract, collapse = "")
  string_xtract
}

#' @name xtend_chb_names
#'
#' @title Reshape REDCap checkbox logic to appropriate R code
#'
#' @description Expand REDCap branching logic and other code that include checkboxes to match indexing format used in underlying repository.
#'
#' @details For code generation, code has to be translated from one DSL or syntax to another.
#'
#' In this case, conversion from REDCap logic to the appropriate R syntax.
#'
#' This function helps map logical expressions from REDCap to R
#'
#' @param x checkbox name in REDCap logic
#'
#' @return R code that can be evaluated after further modifications
#'

xtend_chb_names = function(x) {
  pad = '___'
  pattern_match = regexpr("\\([0-9]{1,}\\)", x)
  pattern_start_pos = as.numeric(pattern_match) + 1
  pattern_start_pos_bckp = as.numeric(pattern_match)
  pattern_len = attr(pattern_match, "match.length") - 3
  pattern_len = pattern_len + pattern_start_pos
  pattern_len_bckp = pattern_start_pos_bckp + attr(pattern_match, "match.length")
  x = rep(x, length(pattern_start_pos))
  replace_numeric_match = function(a, b, c) {
    value = substr(a, b, c)
    if (!is.na(as.integer(value))) {
      value = paste0(pad, value)
      substr(a, pattern_start_pos_bckp, pattern_len_bckp) = value
    }
    return(a)
  }
  mapply(replace_numeric_match, x, pattern_start_pos, pattern_len)
}

#' @name convert_space2tab
#'
#' @title Replace space with tabs
#'
#' @description Format code by replacing spaces with tabs to allow for indentation
#'
#' @details For proper code generation, code that is autogenerated must be availoable for debugging when required.
#'
#' To allow for this requirement, most of the code has to be indented in the format the IDE in use would have indented if written manually.
#'
#' For this, spaces have to be tabbed as required by RStudio, our editor of choice.
#'
#' @param x a string containing R source code
#'
#' @return Properly indented R code


convert_space2tab = function(x) {
  if (regexpr("^[ \t]+", x) > 0) {
    stop_val = attr(regexpr("^[ \t]+", x), "match.length")
    substr(x, 1, stop_val) = paste(rep("\t", nchar(substr(x, 1, stop_val))), collapse = "")
  }
  x
}

#' @name convert_dates_red2r
#'
#' @title Convert REDCap string dates to R dates (code preparation).
#'
#' @description Date conversion from REDCap to R.
#'
#' @details In REDCap logic, dates are represented as raw strings.
#'
#' This is unlike R which requires objects to be of class `Date` for date-like operations to be carried out on the object.
#'
#' This is why conversion to date-operations compatible code is necessary.
#'
#' @param x a string containing R / REDCap source code.
#'
#' @return a string with R/ REDCap source code that has raw string dates replaced with formatted dates.
#'

convert_dates_red2r = function(x) {
  pattern_match = regexpr("'[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}'", x)
  if (pattern_match > 0) {
    tmp = substr(x, pattern_match, (pattern_match + 11))
    newVal = paste0("as.Date(\"", substr(tmp, 2, nchar(tmp) - 1), "\")")
    start_part = substr(x, 1, pattern_match - 1)
    end_part = substr(x, pattern_match + 12, nchar(x))
    x = paste0(start_part, newVal, end_part)
  }
  if (regexpr("'[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}'", x) > 0)
    x = convert_dates_red2r(x)
  x
}

#' @name toproper
#'
#' @export
#'
#' @title Convert strings to proper case.
#'
#' @description Convert strings to proper case.
#'
#' @param x a string.
#'
#' @return a string formatted to proper case
#'

toproper = function(x, all = FALSE) {
  to_proper_case = function(w) {
    if (length(w) != 1L)
      stop("Only one word at at time!")
    w = as.character(w)
    first = toupper(substr(w, 1, 1))
    last = tolower(substr(w, 2, nchar(w)))
    paste0(first, last)
  }
  if (!all) {
    x = to_proper_case(x)
  }
  else {
    x = sapply(strsplit(x, " ")[[1L]], function(w) {
      w = to_proper_case(w)
    })
    x = paste0(x, collapse = " ")
  }
  x
}


#' @name convert_space2tab
#'
#' @title Replace space with tabs
#'
#' @description Format code by replacing spaces with tabs to allow for indentation
#'
#' @details For proper code generation, code that is autogenerated must be availoable for debugging when required.
#'
#' To allow for this requirement, most of the code has to be indented in the format the IDE in use would have indented if written manually.
#'
#' For this, spaces have to be tabbed as required by RStudio, our editor of choice.
#'
#' @param x a string containing R source code
#'
#' @return Properly indented R code


convert_space2tab = function(x) {
  if (regexpr("^[ \t]+", x) > 0) {
    stop_val = attr(regexpr("^[ \t]+", x), "match.length")
    substr(x, 1, stop_val) = paste(rep("\t", nchar(substr(x, 1, stop_val))), collapse = "")
  }
  x
}

#' @name convert_redcap2r
#'
#' @title Convert REDCap string dates to valid R code.
#'
#' @export
#'
#' @description Convert REDCap string dates to valid R code for proper code generation.
#'
#' @details REDCap data dictionaries contain logic encapsulated in the branching logic that affects data capture and hence analysis.
#'
#' These logic has to be transformed into the appropriate analytical tool's DSL for consistent data analysis.
#'
#' In our case, conversion of REDCap logic to R code.
#'
#' This is a composite of many smaller internal functions.
#'
#' @param x a string containing REDCap logic.
#'
#' @return a string with containing R code that can be evaluated in its current context
#'
#'

convert_redcap2r = function(x) {
  if (!is.na(x)) {
    x = convert_dates_red2r(x)
    x = gsub("\\[|\\]", "", x)
    x = gsub("[ \t]+((AND)|(and))[ \t]+", " & ", x)
    x = gsub("[ \t]+((OR)|(or))[ \t]+", " | ", x)
    x = gsub("={1}[ \t]*'", " == '", x)
    x = gsub("(<>)[ \t]*'", " != '", x)
    x = logical_xpressions_red2r(xtend_chb_names(x))
    x = gsub("[ \t]{2, }", " ", x)
  }
  x
}
