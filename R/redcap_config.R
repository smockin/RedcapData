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

redcap_config = setRefClass(
  "RedcapConfig",
  fields = list(
    updates = "list",
    configs = "environment"
  ),

  methods = list(

    show = function() {
      msg = character()
      cnt = length(.self$updates)
      if (cnt == 0) {
        msg = c(msg, "Redcap configurations with no update info(s) <class:RedcapConfig>\n")
      } else {
        msg = c(msg, paste0("Redcap configurations with ", cnt, " updates info(s) <class:RedcapConfig>"))
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
            "...",
            sapply(.self$updates[(cnt)-3, cnt], get_summary_update)
          )
        }
        tmp = paste0(tmp, collapse = "\n")
        msg = paste0(msg, "\nUpdates:\n", tmp)
      }
      cat(msg)
    },

    configs = function() {
      out = as.list(.self$config)
      out$custom_code = if(!is.na(out$custom_code)) {
        "Has custom code"
      } else {
        "No cutsom code specified"
      }
      out$exclusion_pattern = if(!is.na(out$exclusion)) {
        "No exclusion pattern"
      } else {
        "Has exclusion pattern(s)"
      }
    },

    is_valid = function() {
      "Checks the validity of the object"
      valid = TRUE
      msg = character()
      if (!all(
        "api_url",
        "local",
        "token",
        "exlusion_pattern",
        "custom_code",
        "chunksize",
        "chunked",
        "date_var",
        "hosp_var"
      ) %in% ls(all = TRUE, envir = .self$config)) {
        msg = c(msg, "Some required configurations are not set")
      }
      if (is.na(.self$configs$local))
        msg = c(msg, "specify whether redcap is local instance")
      if (!is.logical(.self$configs$local))
        msg = c(msg, "invalid local configuration")
      if (length(.self$configs$local) == 0L)
        .self$configs$local <<- TRUE
      if (length(.self$configs$local) > 1L) {
        warning("local is of length > 1, taking first element")
        .self$configs$local <<- .self$configs$local[1L]
      }
      tmp = "http://localhost/redcap/api/"
      if (.self$configs$local) {
        if (isTRUE(.self$configs$api_url != tmp))
          message("local=T, resetting api_url=", tmp)
        .self$configs$api_url = tmp
      } else {
        if (is.na(.self$configs$api_url))
          message("specify api url")
        if (str_trim(.self$configs$api_url) %in% c("", "NA"))
          stop("specifiy a valid api url")
        .self$configs$api_url <<- str_trim(.self$configs$api_url)
        if (api_url==tmp) {
          message("api_url=", tmp ,", resetting local=TRUE")
          .self$configs$local <<- TRUE
        }
        if (!grepl("/api/$", .self$configs$api_url)) {
          stop("api_url invalid. ???<must end with \"/api/\">???")
        }
      }
      if (is.na(token))
        stop("specify token")
      if (str_trim(token) == "")
        stop("specifiy a valid api token")
      if (!is.character(.self$configs$custom_code))
        stop("invalid custom code")
      if (!is.na(exclusion_pattern))
        if (!is.character(exclusion_pattern))
          stop("invalid exclusion pattern")
      if (length(.self$updates) != 0L) {
        if (!all("RedcapUpdate" %in% sapply(.self$updates, class))) {
          idx = which("RedcapUpdate" %in% sapply(.self$updates, class))
          upds = .self$updates[idx]

        }
        stop("invalid updates")
      }
      config = redcap_config$new(updates = updates)
      config$configs$api_url = api_url
      config$configs$token = token
      config$configs$local = local
      config$configs$exclusion_pattern = exclusion_pattern
      config$configs$api_url = custom_code
      if (!is.valid(config))
        stop("invalid config")
      obj = redcap_class$new(opts = configs)
      obj
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
    }
    )
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
      if (!.self$is_valid()) {
        msg = c(msg, paste0("<!! Note: Update object is invalid! !!>"))
      }
      msg = paste0(msg, collapse = "\n")
      msg = paste0(msg, "\n")
      cat(msg)
    },

    is_valid = function() {
      msgs = character()
      value = TRUE
      if (!is.data.frame(.self$site_info))
        c(msgs, "invalid site info [must be data frame]")
      site_info <<- as.data.frame(.self$site_info)
      if (!length(.self$))
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
