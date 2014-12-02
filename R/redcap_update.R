#' @name redcap_update_list
#'
#' @title Redcap Updates Collection
#'
#' @concept update redcap
#'
#' @description This class holds a collection of update info objects that control the error reporting process.
#'
#'
#' @details It holds the collection of objects that control the error reporing code generation.
#'
#' It also has collection-based operations.
#'
#' @export
#'
#' @field updates a list of update objects.
#'
#' @return A redcap update collection class
#'
#' @family update objects
#'

redcap_update_list = setRefClass(
  "RedcapUpdates",
  fields = list(
    updates = "list"
  ),

  methods = list(

    show = function() {
      msg = character()
      cnt = length(.self$updates)
      if (cnt == 0) {
        msg = c(msg, "An empty collection of updates info(s) <class:RedcapUpdates>\n")
      } else {
        msg = c(msg, paste0("A collection of ", cnt, " updates info(s) <class:RedcapUpdates>"))
        get_summary_update = function(upd) {
          msg = character()
          if (upd$is_valid()) {
            msg = c(msg, paste0(upd$name, ": ", length(upd$new_vars), " new variables (", nrow(upd$site_info), " site(s))"))
          } else {
            msg = c(msg, paste0(upd$name, ": ", " invalid!"))
          }
          msg = paste0(msg, collapse = "\n")
          msg
        }
        if (cnt <= 8) {
          tmp = sapply(.self$updates, get_summary_update)
        } else {
          tmp = c(
            sapply(.self$updates[1:4], get_summary_update),
            sapply(.self$updates[(cnt)-3, cnt], get_summary_update)
          )
        }
        tmp = paste0(tmp, collapse = "\n")
        msg = paste0(msg, "\nUpdates:\n", tmp)
      }
      cat(msg)
    },

    is_valid = function() {
      "Checks the validity of the object"

      browser()
      msg = character()
      value = TRUE
      if (length(.self$updates) > 0) {
        if (!all("RedcapUpdate" %in% sapply(.self$updates, class))) {
          invalid = which(!"RedcapUpdate" %in% sapply(.self$updates, class))
          invalid = paste0(valid, collapse = ", ")
          mgs = c(msg, paste0("items [", invalid, "] not redcap updates!"))
          value = FALSE
        }
        if (!all(sapply(.self$updates, function(up) up$is_valid()))) {
          invalid = which(!(sapply(.self$updates, function(up) up$is_valid())))
          invalid = paste0(valid, collapse = ", ")
          mgs = c(msg, paste0("items [", invalid, "] invalid!"))
          value = FALSE
        }
        if (length(msg) > 0) {
          msg = paste0(msg, collapse = "\n")
          message(msg)
        }
      }
      value
    },

    get_update_date = function(var_name, hospital_id) {
      "Get the date a specific variable was updated for a specific site"

      value = as.Date(NA)
      if (length(.self$updates) > 0) {
        if (!.self$is_valid())
          stop("invalid updates")
        idx = which(sapply(.self$updates, function(x) {
          var_name %in% x$new_vars
        }))
        if (length(idx) > 1)
          stop("new variable in multiple updates")
        if (length(idx) > 0) {
          value = .self$updates[[idx]]
          value = value$get_update_date(var_name, hospital_id)
        }
      }
      value
    })
)

#' @name redcap_update
#'
#' @title Redcap Updates Object
#'
#' @concept update redcap
#'
#' @description This class holds the update information that affects the error reporting process.
#'
#'
#' @details It holds the site information and the date of updates hence it is possible to do site-based operations.
#'
#' The dates can be accessed to allow for conditional generation of the error reports.
#'
#' @export
#'
#' @field name Name of the update.
#' @field site_info A dataset of site information. It has variables site and date matching the date of the update for each site.
#' @field new_vars New variables added during the update.
#'
#' @return A redcap update class
#'
#' @family update objects
#'
#' @include data_types.R
#'

redcap_update = setRefClass(
  "RedcapUpdate",
  fields = list(
    name = "character",
    site_info = "data.frame",
    new_vars = "character"
  ),

  methods = list(

    show = function() {
      msg = "A REDCap update info <class:RedcapUpdate>\n"
      msg = c(msg, paste0("Name: ", .self$name))
      msg = c(msg, paste0("No of sites: ", .self$name))
      msg = c(msg, paste0("Variables Added: { ", paste0(.self$new_vars, collapse = ", "), " }"))
      if (.self$is_valid()) {
        msg = c(msg, paste0("<!! Note: Update object is invalid! !!>"))
      }
      msg = paste0(msg, collapse = "\n")
      msg = paste0(msg, "\n")
      cat(msg)
    },

    is_valid = function() {
      "Checks whether the info is valid"

      msg = character()
      value = TRUE
      if (length(.self$name) < 1) {
        msg = c(msg, "specifiy update name")
        value = FALSE
      }
      if (length(.self$name) > 1) {
        msg = c(msg, "more than one name")
        value = FALSE
      }
      if (!is.data.frame(.self$site_info)) {
        msg = c(msg, "site_info must be a data frame")
        value = FALSE
      }
      if (!all(c("site", "date") %in% colnames(.self$site_info))) {
        msg = c(msg, "[site, date] not in site_info")
        value = FALSE
      }
      if (all(!sapply(.self$site_info$site, is_int))) {
        msg = c(msg, "some sites [hosp_id] not integer")
        value = FALSE
      }
      if (!all(sapply(.self$site_info$date, is_date))) {
        msg = c(msg, "some dates [update_dates] not valid [YYYY-MM-DD]")
        value = FALSE
      }
      if (!length(.self$site_info$site) == length(unique(.self$site_info$site))) {
        msg = c(msg, "some sites [hosp_id] have duplicates")
        value = FALSE
      }
      if (length(.self$new_vars) == 0) {
        msg = c(msg, "No new variables")
        value = FALSE
      }
      if (length(.self$new_vars) != length(unique(.self$new_vars))) {
        msg = c(msg, "duplicates in new variables")
        value = FALSE
      }
      if (length(msg) > 0) {
        msg = paste0(msg, collapse = "\n")
        message(msg)
      }
      value
    },

    get_update_date = function(var_name, hospital_id) {
      "Get the date a specific variable was updated for a specific site"

      if (!.self$is_valid())
        stop("invalid update!")
      value = as.Date(NA)
      idx = which(var_name %in% .self$new_vars)
      if (length(idx) > 0) {
        value = as.Date(.self$site_info[.self$site_info$site == hospital_id, "date"][1])
      }
      value
    }
  )
)
