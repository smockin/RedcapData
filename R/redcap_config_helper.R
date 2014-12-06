#' @include redcap_config.R
NULL

#' @name load_updates
#'
#' @title Wrapper for creating REDCap update objects
#'
#' @description Cleaner way for instantiating REDCap update objects.
#'
#' It uses an R data frame to instantiate update objects.
#'
#' This avoids messing up instantiation in R6 classes which is more complex and error prone.
#'
#' @details This function acts as a wrapper for instantiating an object that abstracts the REDCap updates.
#'
#' It performs the necessary checks for instantiation hence avoids messy objects.
#'
#' It then calls the new method of the underlying reference class.
#'
#' The update dataset must contain the following variables:
#'
#' \describe{
#'  \item{update}{The update name of format mon-yy, eg Nov-14, Feb-13}
#'  \item{site}{The hospital id of the site, must be integer and must be replicated for each update}
#'  \item{date}{The date of the update for each specific site}
#'  \item{new_vars}{
#'  The new variables introduced during the update <must have at least one entry per update>.
#'
#'  The first non misssing entry will be used.
#'
#'  The variable names must be separated by semi-colons.
#'  }
#' }
#'
#' @param updates_data A dataset containing the updates info. For the column specifications, see details.
#'
#' @export
#'
#' @seealso \code{\link{redcap_update}}
#'
#' @return A list of redcap update class objects.
#'


load_updates = function(updates_data) {
  if (!is.data.frame(updates_data))
    stop("updates_data not a data frame")
  updates_data = data.frame(updates_data)
  if (any(is.na(updates_data$update) | str_trim(updates_data$update) == ""))
    stop("some updates(s) missing")
  if (!all(c("update", "site", "date", "new_vars") %in% names(updates_data)))
    stop("some column(s) missing")
  if (any(is.na(updates_data$site) | str_trim(updates_data$site) == ""))
    stop("some site(s) missing")
  if (any(is.na(updates_data$date) | str_trim(updates_data$date) == ""))
    stop("some date(s) missing")
  if (!all(sapply(str_trim(updates_data$site), is_int)))
    stop("some site(s) are invalid")
  updates_data$site = as.integer(updates_data$site)
  if (!all(sapply(str_trim(updates_data$date), is_date)))
    stop("some date(s) are invalid")
  updates_data$date = as.Date(updates_data$date)
  value = list()
  updates_data = split(updates_data, updates_data$update)
  updates = names(updates_data)
  value = lapply(updates, function(up_name) {
    update_curr = updates_data[[up_name]]
    name = up_name
    site_info = update_curr[, c("site", "date")]
    if (length(unique(site_info$site)) != length(site_info$site))
      stop(paste0("some sites duplicated [", name, "]"))
    new_vars = as.character(update_curr[, "new_vars"])
    new_vars[str_trim(new_vars) == ""] = NA
    new_vars = na.omit(new_vars)[1]
    new_vars = str_trim(unlist(strsplit(new_vars, ";")))
    new_vars = unique(new_vars)
    redcap_update$new(name = name, site_info = site_info, new_vars = new_vars)
  })
  value
}

#' @name load_configs
#'
#' @title Wrapper for creating REDCap configuration objects
#'
#' @description Cleaner way for instantiating REDCap configuration objects.
#'
#' It uses an R data frame and character vectors to instantiate REDCap configuration objects.
#'
#' This avoids messing up instantiation in R6 classes which is more complex and error prone.
#'
#' @details This function acts as a wrapper for instantiating an object that abstracts the REDCap updates.
#'
#' It performs the necessary checks for instantiation hence avoids messy objects.
#'
#' It then calls the new method of the underlying reference class.
#'
#' The update dataset must contain the following variables:
#'
#' \describe{
#'  \item{update}{The update name of format mon-yy, eg Nov-14, Feb-13}
#'  \item{site}{The hospital id of the site, must be integer and must be replicated for each update}
#'  \item{date}{The date of the update for each specific site}
#'  \item{new_vars}{
#'  The new variables introduced during the update <must have at least one entry per update>.
#'
#'  The first non misssing entry will be used.
#'
#'  The variable names must be separated by semi-colons.
#'  }
#' }
#'
#' @param updates_data A dataset containing the updates info. For the column specifications, see details.
#'
#' @export
#'
#' @seealso \code{\link{redcap_update}}
#'
#' @return A list of redcap update class objects.
#'

load_configs = function(config_data = NULL, custom_code = NA, exclusion_pattern = NA) {
  tmp = config_keys
  if (!is.null(config_data)) {
    if (all(!is.data.frame(config_data) | is.matrix(config_data)))
      stop("invalid config data")
    config_data = as.data.frame(config_data)
    if (!all(c("key", "value", "type") %in% names(config_data)))
      stop("config data should have key, value and data type")
    config_data = config_data[, c("key", "value", "type")]
    if (any(duplicated(config_data$key)))
      stop("duplicate keys in config data")
    config_data = config_data[config_data$key %in% names(config_keys),]
    config_data = data.frame(sapply(config_data) function(x) {
      str_trim(x)
    })
    if (nrow(config_data) > 0) {
      invisible(sapply(config_data$key, function(k) {
        cnf = config_data[config_data$key == k,]
        ky = cnf$key
        typ = cnf$type
        val = cnf$value
        val = if (typ == "date") {
          if (str_trim(val) %in% c("NA", "")) {
            val = as.Date(NA)
          } else (!is_date(val)) {
            stop(paste0(sQuote(ky), " must be a date (format:yyyy-mm-dd)"))
          }
          as.Date(val)
        } else if (typ == "float") {
          if (str_trim(val) %in% c("NA", "")) {
            val = NA_real_
          } else (!is_number(val)) {
            stop(paste0(sQuote(ky), " must be a float"))
          }
          as.numeric(val)
        } else if (type == "integer") {
          if (str_trim(val) %in% c("NA", "")) {
            val = NA_integer_
          } else (!is_integer(val)) {
            stop(paste0(sQuote(ky), " must be an integer"))
          }
          as.integer(val)
        } else if (type == "boolean") {
          if (str_trim(val) %in% c("NA", "")) {
            val = NA
          } else (!is_boolean(val)) {
            stop(paste0(sQuote(ky), " must be a boolean"))
          }
          as.logical(val)
        }
        tmp[[k]] = val
      }))
    }
  }
  if (!is.na(custom_code))
    if (!is.character(custom_code))
      stop("invaid custom code")
  if (!is.na(exclusion_pattern))
    if (!is.character(exclusion_pattern))
      stop("invalid exclusion pattern")
  tmp$custom_code = custom_code
  tmp$exclusion_pattern = exclusion_pattern
  obj = redcap_config$new(config = as.environment(tmp))
  if (!obj$is_valid())
    stop("configs not set")
  obj
}

config_keys = list(
  api_url = "http://localhost/redcap/api/",
  token = "",
  local = "true",
  chunked = FALSE,
  chunksize = NA,
  hosp_var = "hosp_id",
  date_var = "date_today",
  hosp_code = NA
  )
