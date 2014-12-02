#' @name tab_utils
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

#' @rdname tab_utils
#'

add_tab = function(n = 1) .__TAB__no <<- .__TAB__no + n

#' @rdname tab_utils
#'

remove_tab = function(n = 1) .__TAB__no <<- max(0, .__TAB__no - n)

#' @rdname tab_utils
#'

reset_tab = function() .__TAB__no <<- 0

#' @rdname tab_utils
#'

get_tab = function() paste0(rep("\t", .__TAB__no), collapse = "")
