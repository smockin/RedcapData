#' @rdname RemoveHtmlTags
#'
#' @name remove_html_tags
#'
#' @title Remove html tags from string
#'
#' @description Removes html tags from string which can later be used as html-free text eg in labels.
#'
#' @details This is a utility function for refining text.
#'
#' This is useful in generic tasks such as autogeneration of code.
#'
#' @param x A string possibly with embedded html tags.
#'
#' @export
#'
#' @return A string free of html tags and elements.
#'
#'
remove_html_tags = function(x) {
  .namesX = names(x)
  value = sapply(x, function(s) { gsub("<[^>]*>", "", s) })
  names(value) = NULL
  value
}

#' @name logical_xpressions_red2r
#'
#' @rdname LogicalExpressionsFromRedcapToR
#'
#' @title Reshape REDCap logical expresssions to R logical code.
#'
#' @description Convert REDCap logical expressions to valid R syntax representations.
#'
#' @details For code generation, code has to be translated from one DSL or syntax to another.
#'
#' In this case, conversion from REDCap logic to the appropriate R syntax.
#'
#' This function helps map logical expressions from REDCap to R
#'
#' @param x A string with REDCap logic, either raw or preprocessed.
#'
#' @return Preprocessed R code that can be refined further or valid R syntax that can be parsed into an expression tree for evaluation.
#'
#' @include data_types.R
#'
#' @family RedcapToR
#'

logical_xpressions_red2r = function(x) {
  pattern_match = regexpr("[ \t]*((==)|(!=))[ \t]*'[\\-]*[a-zA-Z0-9]*'", x)
  if (pattern_match > 0) {
    len_match = attr(pattern_match, "match.length") + pattern_match
    start_part = substr(x, 1, pattern_match - 1)
    var_part = gregexpr("[ \t]*[\\-]*[a-zA-Z0-9_]+[ \t]*", start_part)[[1]]
    len_part = attr(var_part, "match.length")
    len_part = len_part[length(len_part)]
    var_part = var_part[length(var_part)]
    var_part = str_trim(substr(start_part, var_part, len_part + var_part + 1))
    match_part = substr(x, pattern_match, len_match - 1)
    end_part = substr(x, len_match, nchar(x))
    has_value = regexpr("'[\\-]*[a-zA-Z0-9]+'", match_part)
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
    if (regexpr("[ \t]*((==)|(!=))[ \t]*'[\\-]*[a-zA-Z0-9]*'", x) > 0L)
      x = logical_xpressions_red2r(x)
    x
  }
  x
}

#' @name convert_missing_red2r
#'
#' @rdname ConvertMissingnessRedcapToR
#'
#' @title Reshape REDCap missingness logic to appropriate R code
#'
#' @description Convert REDCap missingness representations to corresponding R syntax.
#'
#' @details For code generation, code has to be translated from one DSL or syntax to another.
#'
#' In this case, conversion from REDCap logic to the appropriate R syntax.
#'
#' This function helps map mssingness expressions from REDCap to R
#'
#' @param x A string with REDCap logic, either raw or preprocessed.
#'
#' @return Preprocessed R code that can be refined further or valid R syntax that can be parsed into an expression tree for evaluation.
#'
#'@include data_types.R
#'
#'@family RedcapToR
#'

convert_missing_red2r = function(x) {
  reshape_na_red2r = function(x) {
    ._i = as.integer(regexpr("[a-z]", tolower(x)))
    start_part = substr(x, 1, ._i - 1)
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


#' @rdname ExtendCheckboxNames
#'
#' @name xtend_chb_names
#'
#' @title Reshape REDCap checkbox logic in REDCap to the appropriate R code
#'
#' @description Expand REDCap branching logic and other code that include checkboxes to match indexing format used in underlying repository.
#'
#' @details For code generation, code has to be translated from one DSL or syntax to another.
#'
#' In this case, conversion from REDCap logic to the appropriate R syntax.
#'
#' This function helps expand REDCap checkbox syntax eg chk(1) to the underlying data format expected in R eg chk___1
#'
#' @param x String with REDCap logic possibly with embedded checkbox references.
#'
#' @return Preprocessed R code that can be refined further or valid R syntax that can be parsed into an expression tree for evaluation.
#'
#' @family RedcapToR

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

#' @rdname ConvertSpacesToTabs
#'
#' @name convert_space2tab
#'
#' @title Replace leading spaces in code files with tabs
#'
#' @description Format code by replacing leading spaces with tabs to allow for block indentation
#'
#' @details For proper code generation, code that is autogenerated must be available for debugging when required.
#'
#' To allow for this requirement, most of the code has to be indented in a format that the IDE in use would have indented it if it was written manually.
#'
#' For this, spaces have to be tabbed as required by RStudio, our editor of choice (bias!!).
#'
#' This utility function is used to format any custom code for the IDE and is purely for aesthetic purposes.
#'
#' @param x A string containing source code
#'
#' @return Properly indented source code


convert_space2tab = function(x) {
  if (regexpr("^[ \t]+", x) > 0) {
    stop_val = attr(regexpr("^[ \t]+", x), "match.length")
    substr(x, 1, stop_val) = paste(rep("\t", nchar(substr(x, 1, stop_val))), collapse = "")
  }
  x
}


#' @rdname ConvertDatesInRedcapToR
#'
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
#' This requires conversion to facilitate date-based operations.
#'
#' @param x A string with REDCap logic, either raw or preprocessed.
#'
#' @return Preprocessed R code that can be refined further or valid R syntax that can be parsed into an expression tree for evaluation.
#'
#' @family RedcapToR

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

#' @rdname ToProperCase
#'
#' @name toproper
#'
#' @export
#'
#' @title Convert strings to proper case.
#'
#' @description Convert strings to proper case.
#'
#' @param x A string.
#' @param all Whether to convert all words. Default is only the first word is converted to proper case, the rest is lower case.
#'
#' @return A string formatted to proper case
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

#' @rdname ConvertRedcapLogicToR
#'
#' @name convert_redcap2r
#'
#' @title Convert REDCap logic to R code.
#'
#' @export
#'
#' @description Wrapper for converting REDCap branching logic to valid R code (for metaprogramming purposes).
#'
#' @details REDCap data dictionaries contain logic encapsulated in the branching logic that affects data capture and hence data management and analysis.
#'
#' These logic(s) has to be transformed into the appropriate analytical tool's DSL for consistent data munging and analysis.
#'
#' In our case, this involves conversion from REDCap logic to R code.
#'
#' This is a composite function made up of an aggregation of many smaller internal functions.
#'
#' @param x A string containing REDCap logic.
#'
#' @return A string with containing valid R code (token) that can be parsed in it's current context into an R expression tree for evaluation.
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
