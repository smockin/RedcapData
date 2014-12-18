#' @name Tab_Utils
#'
#' @title Manage Tabbing in script creation
#'
#' @description Indent code for easier reading and debugging.
#'
#' @details These help in code generation.
#'
#' Proper use of these functions result in well indented scripts during metaprogrammiung.
#'
NULL

#' @rdname Tab_Utils
#' @param n Number of tabs to add

add_tab = function(n = 1) {
  tabs = get(".__TAB__no", envir = globalenv())
  tabs = tabs + n
  assign(".__TAB__no", tabs, envir = globalenv())
}

#' @rdname Tab_Utils
#' @param n Number of tabs to remove

remove_tab = function(n = 1) {
  tabs = get(".__TAB__no", envir = globalenv())
  tabs = max(0, tabs - n, na.rm = TRUE)
  assign(".__TAB__no", tabs, envir = globalenv())
}

#' @rdname Tab_Utils
#'

reset_tab = function() assign(".__TAB__no", 0, envir = globalenv())

#' @rdname Tab_Utils
#'

get_tab = function() paste0(rep("\t", get(".__TAB__no", envir = globalenv())), collapse = "")
