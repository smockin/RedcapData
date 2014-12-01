add_tab = function() .__TAB__no <<- .__TAB__no + 1
remove_tab = function() .__TAB__no <<- max(0, .__TAB__no - 1)
reset_tab = function() .__TAB__no <<- 0
get_tab = function() paste0(rep("\t", .__TAB__no), collapse = "")
