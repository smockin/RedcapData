#' @include redcap_config.R
NULL

#' @rdname LoadUpdates
#'
#' @name load_updates
#'
#' @title Wrapper for creating REDCap update objects
#'
#' @description Cleaner way for instantiating RedcapUpdate object(s).
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
#' @param updates_data A dataset containing the updates info. For the column specifications, see Details.
#'
#' @seealso \code{\link{RedcapUpdate}} \code{\link{Redcap}}, \code{\link{redcap_project}}
#'
#' @return A list of redcap update class objects.
#'
#' @family Configuration Objects


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
    RedcapUpdate$new(name = name, site_info = site_info, new_vars = new_vars)
  })
  value
}

#' @rdname LoadConfigurations
#'
#' @name load_configs
#'
#' @title Wrapper for creating REDCap configuration objects
#'
#' @description Cleaner way for instantiating REDCap configuration objects.
#'
#' It uses an R data frame and character vectors to instantiate REDCap configuration objects.
#'
#' This abstracts instantiation in R6 classes which is more complex and error prone.
#'
#' @details This function acts as a wrapper for instantiating an object that abstracts the REDCap configurations.
#'
#' It performs the necessary checks for instantiation hence avoids messy objects.
#'
#' It then calls the new method of the underlying reference class.
#'
#' The configuration dataset must contain the following variables:
#'
#' \describe{
#'  \item{key}{The name of the configuration}
#'  \item{value}{The value the configuration takes}
#'  \item{type}{The data type for the configuration. Its either `string`, `number`, `integer`, `date` or `boolean`}
#' }
#'
#' Some common configuations include:
#'
#' \describe{
#'  \item{api_url}{The url to the redcap api.
#'
#'  Just append /api/ to the REDCap instance's url link. [type : `string`].
#'
#'  Defaults to http://localhost/redcap/api/ if not specified.
#'
#'  }
#'  \item{token}{Token necessary for authentication.
#'
#'  Must have api rights to obtain this from REDCap. [type : `string`].
#'
#'  Defaults to an empty string if not specified. This is invalid and therefore is the only configuration that must be specified.
#'  }
#'  \item{local}{Whether REDCap instance is local to the machine. [type: `boolean`].
#'
#'  Defaults to TRUE if not specified.
#'  }
#'  \item{chunked}{Whether to pull data from REDCap / Validate data entry in chunks. This helps if the data size is large or in cases of network latency [type: `boolean`].
#'
#'  Defaults to FALSE if not specified hence data operations are carried out in bulk by default.
#'
#'  }
#'  \item{chunksize}{The size of the chunks to be used. Depends on the data size [type: `integer`].
#'
#'  This is ignored if chunked is FALSE but must be provided in case chunked is TRUE.
#'
#'  }
#'  \item{date_var}{The name of the variable that holds the date of record entry [type: `string`].
#'
#'  This defaults to date_today in line with CIN's data governance framework if not specified.
#'  }
#'  \item{hosp_var}{The name of the variable that holds the date of record entry [type: `string`]
#'
#'  This defaults to hosp_id in line with CIN's data governance framework if not specified.
#'  }
#'  \item{report_location}{The path to the generated error report [type: `string`]
#'
#'  This defaults to a temporary file in a subdirectory of the default temporary directory if not specified.
#'  }
#'  \item{hosp_to_validate}{The id of the hospital to generate an error report on. [type: `integer`].
#'
#'  During error reporting, if an entry's hospital id does not match this code a data entry error is raised.
#'
#'  This defaults to NA if not specified.
#'  }
#' }
#'
#' @param config_data A dataset containing the configurations. For the column specifications and some common entries, see Details.
#' @param custom_code A character vector containing custom code for error reporting. If located in a file use \code{readLines} to load the contents to a character vector
#' @param exclusion_pattern A character vector containing regex patterns for variables to be excluded from error reporting.
#'
#' The exclusion may be because these variables are handled specially in the custom code or do not add value to the error reporting process.
#'
#' @seealso \code{\link{RedcapUpdate}}, \code{\link{Redcap}}, \code{\link{redcap_project}}
#'
#' @return A RedcapConfig object.
#'
#' @family Configuration Objects

load_configs = function(config_data = NULL, custom_code = NA, exclusion_pattern = NA) {
  tmp = config_keys
  if (!is.null(config_data)) {
    if (all(!is.data.frame(config_data) | is.matrix(config_data)))
      stop("invalid config data")
    config_data = as.data.frame(config_data, stringsAsFactors = TRUE)
    if (!all(c("key", "value", "type") %in% names(config_data)))
      stop("config data should have key, value and data type")
    config_data = config_data[, c("key", "value", "type")]
    if (any(duplicated(config_data$key)))
      stop("duplicate keys in config data")
    config_data = config_data[config_data$key %in% names(config_keys),]
    config_data = data.frame(sapply(config_data, function(x) {
      str_trim(x)
    }), stringsAsFactors = TRUE)
    if (nrow(config_data) > 0) {
      invisible(sapply(config_data$key, function(k) {
        cnf = config_data[config_data$key == k,]
        ky = as.character(cnf$key)
        typ = as.character(cnf$type)
        val = as.character(cnf$value)
        val = if (typ == "date") {
          if (str_trim(val) %in% c("NA", "")) {
            as.Date(NA)
          } else if (!is_date(val)) {
            stop(paste0(sQuote(ky), " must be a date (format:yyyy-mm-dd)"))
          }
          as.Date(val)
        } else if (typ == "number") {
          if (str_trim(val) %in% c("NA", "")) {
            NA_real_
          } else if (!is_number(val)) {
            stop(paste0(sQuote(ky), " must be a float"))
          }
          as.numeric(val)
        } else if (typ == "integer") {
          if (str_trim(val) %in% c("NA", "")) {
            NA_integer_
          } else if (!is_int(val)) {
            stop(paste0(sQuote(ky), " must be an integer"))
          }
          as.integer(val)
        } else if (typ == "boolean") {
          if (str_trim(val) %in% c("NA", "")) {
            NA
          } else if (!is_boolean(val)) {
            stop(paste0(sQuote(ky), " must be a boolean"))
          }
          as.logical(val)
        } else if (typ == "string") {
          if (str_trim(val) %in% c("NA", ""))
            NA_character_
          else
            as.character(val)
        } else {
          stop("config data type not supported!. must be either `string`, `date`, `integer`, `number` or `boolean`")
        }
        tmp = get("tmp", envir = parent.frame(n = 3))
        tmp[[ky]] = val
        tmp = assign("tmp", tmp, envir = parent.frame(n = 3))
      }))
    }
  }
  if (!all(sapply(custom_code, is.na)))
    if (!is.character(custom_code))
      stop("invalid custom code")
  if (!all(sapply(exclusion_pattern, is.na)))
    if (!is.character(exclusion_pattern))
      stop("invalid exclusion pattern")
  tmp$custom_code = custom_code
  tmp$exclusion_pattern = exclusion_pattern
  obj = RedcapConfig$new(configs = as.environment(tmp))
  if (!obj$is_valid())
    stop("configs not set")
  obj
}

#' @rdname ConfigurationKeys
#'
#' @name config_keys
#'
#' @title Default Configuration Keys
#'
#' @description Default keys for Redcap configuration
#'
#' @details This serves as the default set of configurations if no value has been specified for some specific key(s).
#'
#' Helps in instantiation of RedcapConfig objects.
#'
#' @return A list with default Redcap configurations as entries.
#'
#' @family Configuration Objects

config_keys = list(
  api_url = "http://localhost/redcap/api/",
  token = "",
  local = TRUE,
  chunked = FALSE,
  chunksize = NA_integer_,
  hosp_var = "hosp_id",
  date_var = "date_today",
  report_location = tempfile("Error_Report", fileext = ".csv"),
  hosp_to_validate = NA_integer_
  )
