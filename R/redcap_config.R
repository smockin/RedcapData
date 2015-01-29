#' @include data_types.R
NULL

#' @name RedcapConfig
#'
#' @title Redcap Configuration Wrapper Class
#'
#' @concept configuration cin redcap
#'
#' @description This class holds redcap configurations that control the mode of interaction with the data repository.
#'
#' @details This configurations affect various processes encapsulated by the REDCap class from data input to error reporting.
#'
#' This allows the configurations to be loosely coupled from the REDCap interaction for both flexibility and maintainability.
#'
#' @export
#'
#' @field updates a list of RedcapUpdate object(s).
#' @field configs an internal environment for holding configurations.
#'
#' @return A Redcap configuration class
#'
#' @family Configuration Objects
#'

RedcapConfig = setRefClass(
  "RedcapConfig",
  fields = list(
    updates = "list",
    configs = "environment"
  ),

  methods = list(

    show = function() {
      if (.self$is_valid()) {
        msg = character()
        cnt = length(.self$updates)
        if (cnt == 0) {
          msg = c(msg, "Redcap configurations with no update info(s)\n")
        } else {
          msg = c(msg, paste0("Redcap configurations with ", cnt, " updates info(s)"))
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
      } else {
        cat("Invalid configurations!\n")
      }
    },

    list_configs = function() {
      "Display the configurations for the REDCap session/object"

      if (.self$is_valid()) {
        out = as.list(.self$configs)
        out$custom_code = if(!all(sapply(out$custom_code, is.na))) {
          "Has custom error reporting code specified"
        } else {
          "No custom error reporting code specified"
        }
        out$exclusion_pattern = if(!all(sapply(out$exclusion, is.na))) {
          "Has exclusion pattern(s) specified"
        } else {
          "No exclusion pattern specified"
        }
        msg = "REDCap Configurations:\n---------------------------\n"
        invisible({
          sapply(1 : length(out), function(idx) {
            nameCnf = names(out)[idx]
            cnf = out[idx]
            msg = get("msg", envir = parent.frame(3))
            msg = c(msg, paste0(">> ", nameCnf, " : ", cnf))
            assign("msg", msg, envir = parent.frame(3))
          })
        })
        msg = c(msg, "")
        msg = paste0(msg, collapse = "\n")
        msg = paste0(msg, "\n")
        cat(msg)
      } else {
        cat("Invalid configurations!\n")
      }
    },

    is_valid = function() {
      "Checks the validity of the object"

      valid = TRUE
      msg = character()
      if (!all(c(
        "api_url",
        "local",
        "token",
        "exclusion_pattern",
        "custom_code",
        "chunksize",
        "chunked",
        "date_var",
        "hosp_var"
      ) %in% ls(all = TRUE, envir = .self$configs))) {
        msg = c(msg, "Some required configurations are not set")
        valid = FALSE
      }
      if (is.na(.self$configs$local)) {
        msg = c(msg, "specify whether redcap is local instance")
        valid = FALSE
      }
      if (!is.logical(.self$configs$local)) {
        msg = c(msg, "invalid local configuration (must be logical)")
        valid = FALSE
      }
      if (length(.self$configs$local) == 0L) {
        warning("local not specified, reseting to TRUE")
        .self$configs$local = TRUE
      }
      if (length(.self$configs$local) > 1L) {
        warning("local is of length > 1, taking first element")
        .self$configs$local = .self$configs$local[1L]
      }
      tmp = "http://localhost/redcap/api/"
      if (.self$configs$local) {
        if (isTRUE(.self$configs$api_url != tmp)) {
          warning("local=T, resetting api_url = ", sQuote(tmp))
          .self$configs$api_url = tmp
        }
      } else {
        if (is.na(.self$configs$api_url)) {
          msg = c(msg, "specify api url")
          valid = FALSE
        }
        if (str_trim(.self$configs$api_url) %in% c("", "NA")) {
          msg = c(msg, "specifiy a valid api url")
          valid = FALSE
        }
        .self$configs$api_url = str_trim(.self$configs$api_url)
        if (.self$configs$api_url == tmp) {
          warning("api_url = ", tmp ,", resetting local=TRUE")
          .self$configs$local = TRUE
        }
      }
      if (!grepl("/api/$", .self$configs$api_url)) {
        msg = c(msg, "api_url invalid. ???<must end with \"/api/\">???")
        valid = FALSE
      }
      if (length(.self$configs$api_url) > 1L) {
        warning("api_url is of length > 1, taking first element")
        .self$configs$api_url = .self$configs$api_url[1L]
      }
      if (is.na(.self$configs$token)) {
        msg = c(msg, "specify token")
        valid = FALSE
      } else if (str_trim(.self$configs$token) == "") {
        msg = c(msg, "specifiy a valid api token")
        valid = FALSE
      } else if (length(.self$configs$token) > 1L) {
        warning("token is of length > 1, taking first element")
        .self$configs$token = .self$configs$token[1L]
      }
      if (!all(sapply(.self$configs$custom_code, is.na))) {
        if (!is.character(.self$configs$custom_code)) {
          msg = c(msg, "invalid custom code")
          valid = FALSE
        }
      }
      if (is.na(.self$configs$chunked)) {
        msg = c(msg, "specify chunked")
        valid = FALSE
      } else if (!is.logical(.self$configs$chunked))  {
        msg = c(msg, "invalid chunked")
        valid = FALSE
      }
      if (.self$configs$chunked) {
        if (is.na(.self$configs$chunksize)) {
          msg = c(msg, "specify chunksize")
          valid = FALSE
        }
        else if (!is.numeric(.self$configs$chunksize)) {
          msg = c(msg, "chunksize invalid")
          valid = FALSE
        }
        else if (.self$configs$chunksize < 0) {
          msg = c(msg, "chunksize invalid")
          valid = FALSE
        }
      }
      if (!all(sapply(.self$configs$exclusion_pattern, is.na)))
        if (!is.character(.self$configs$exclusion_pattern)) {
          msg = c(msg, "invalid exclusion pattern")
          valid = FALSE
        }
      if (length(.self$updates) != 0L) {
        if (!all("RedcapUpdate" %in% sapply(.self$updates, class))) {
          idx = which("RedcapUpdate" %in% sapply(.self$updates, class))
          upds = .self$updates[idx]
          upds = sQuote(sapply(upds, function(up) up$name))
          upds = paste0(upds, collapse = ", ")
          msg = c(msg, paste0("invalid updates <", upds, ">"))
          valid = FALSE
        } else if (!all(sapply(.self$updates, function(x) x$is_valid()))) {
          idx = which(all(!sapply(.self$updates, function(x) x$is_valid())))
          upds = .self$updates[idx]
          upds = sQuote(sapply(upds, function(up) up$name))
          upds = paste0(upds, collapse = ", ")
          msg = c(msg, paste0("invalid updates <", upds, ">"))
          valid = FALSE
        }
      }
      if (!valid) {
        msg = paste0(msg, collapse = "\n")
        msg = paste0(msg, "\n")
        cat(msg)
      }
      return(valid)
    },

    get_update_date = function(var_name, hospital_id) {
      "Get the date a specific variable was updated for a specific site"

      value = as.Date(NA)
      if (length(.self$updates) > 0) {
        if (!all(sapply(.self$updates, function(x) x$is_valid())))
          stop("Some updates not valid")
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

#' @name RedcapUpdate
#'
#' @title Redcap Update Wrapper Class
#'
#' @concept updates configuration cin redcap
#'
#' @description This class holds the update information that affects the error reporting process.
#'
#' @details It holds the site information and the date of updates hence it is possible to do site-based operations.
#'
#' The dates can be accessed to allow for conditional generation of the error reports.
#'
#' @export
#'
#' @field name Name of the update.
#' @field site_info A dataset of site information. It has variables `site` and `date` matching the site identifier and date of the update for each site.
#' @field new_vars New variables introduced during the update.
#'
#' @return A redcap update class
#'
#' @family Configuration Objects
#'

RedcapUpdate = setRefClass(
  "RedcapUpdate",
  fields = list(
    name = "character",
    site_info = "data.frame",
    new_vars = "character"
  ),

  methods = list(

    show = function() {
      msg = "REDCap update info\n"
      msg = c(msg, paste0("Name: ", .self$name))
      msg = c(msg, paste0("No of sites: ", nrow(.self$site_info)))
      msg = c(msg, paste0("Variables Added: \n[ ", paste0(.self$new_vars, collapse = ", "), " ]"))
      if (!.self$is_valid()) {
        msg = c(msg, paste0("!! Note: Update object is invalid! !!"))
      }
      msg = paste0(msg, collapse = "\n")
      msg = paste0(msg, "\n")
      cat(msg)
    },

    is_valid = function() {
      "Check validity of object"

      msgs = character()
      valid = TRUE
      if (!is.data.frame(.self$site_info)) {
        c(msgs, "invalid site info (must be data frame)")
        valid = FALSE
      }
      .self$site_info = as.data.frame(.self$site_info)
      if (!length(.self$name > 1)) {
        warning("update name is of length > 1")
        .self$name = .self$name[1L]
      }
      valid
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
