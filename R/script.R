#' @rdname TabUtils
#'
#' @name TabUtils
#'
#' @title Manage tabs when autogenerating scripts
#'
#' @param n Number of tabs to add / remove
#'
#' @description Indent code for easier reading and debugging especially in IDEs.
#'
#' @details These help in code generation when metaprogramming.
#'
#' Proper use of these functions result in well indented scripts.
#'
NULL

#' @rdname TabUtils

add_tab = function(n = 1) {
  tabs = get(".__TAB__no", envir = globalenv())
  tabs = tabs + n
  assign(".__TAB__no", tabs, envir = globalenv())
}

#' @rdname TabUtils

remove_tab = function(n = 1) {
  tabs = get(".__TAB__no", envir = globalenv())
  tabs = max(0, tabs - n, na.rm = TRUE)
  assign(".__TAB__no", tabs, envir = globalenv())
}

#' @rdname TabUtils
#'

reset_tab = function() assign(".__TAB__no", 0, envir = globalenv())

#' @rdname TabUtils
#'

get_tab = function() paste0(rep("\t", get(".__TAB__no", envir = globalenv())), collapse = "")
