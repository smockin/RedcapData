#' @include redcap_helper.R
NULL


#' @name redcap_class
#'
#' @title Redcap Wrapper Class
#'
#' @aliases redcap_wrapper redcap_project cin_project
#'
#' @concept cin
#'
#' @description This class is the entry point for interacting with the CIN data which is stored in a REDCap data repository
#'
#' @details This object facilitates R's interface with the REDCap repository.
#'
#' Data can be pulled either in bulk or chunked.
#'
#' Currently only metadata and records can be pulled.
#'
#' It also provides other data allied tasks such as data cleaning, error reporting and formatting.
#'
#' It uses a cache system and logs major events
#'
#' Show the object to view status
#'
#' @export
#'
#' @field api The url to the project's web based api. Just append /api/ to the project's url.
#' @field token The secret token for the project. Check the API page in REDCap. Must have api rights to access this.
#' @field local Whether REDCap instance is local
#' @field exclusion_pattern regex for variables to exclude from autogeneration of code
#' @field custom_code Custom error report code
#'
#' @return A redcap class with an array of functionality
#'
#' @seealso \code{\link{redcap}}
#'
#' @include redcap_update.R
#'
#' @section Info:
#' Use redcap function to instantiate this class. This avoids many pitfalls during the lifetime of this object.

redcap_class = setRefClass(
  "Redcap",
  fields = list(
    .__cache = "environment",
    .__log = "character",
    api = "character",
    token = "character",
    local = "logical",
    exclusion_pattern = "character",
    custom_code = "character"
  ),

  methods = list(

    show = function() {
      www = gsub("/api/", "", .self$api)
      if (.self$local)
        msg = "redcap:\nA local redcap instance <class:redcap>\n"
      else
        msg = paste0("redcap:\nA remote redcap instance running at ", sQuote(www), " <class:redcap>\n")
      if (length(ls(.self$.__cache)) == 0) {
        msg = c(msg, "memory: cache empty\n")
      }
      else {
        msg = c(msg, paste0("memory: cache contains ", length(ls(.self$.__cache)), " items\n"))
      }
      msg = c(msg, paste0("events:", get_status(ls(.self$.__cache), pretty = TRUE)))
      if (length(.self$.__log) > 0)
        msg = c(msg, paste0("log:\n", .self$.__log))
      msg = paste0(msg, collapse = "\n")
      cat(msg)
    },

    load_data = function(chunked = FALSE, chunksize = NULL, forms = NULL, fields = NULL, ids_to_pull = NULL) {
      "Stream data from REDCap if not in cache"

      rm(list = ls(.self$.__cache), envir = .self$.__cache)
      if (chunked) {
        if (!is.numeric(chunksize))
          .self$log("chunksize not specified yet chunked=TRUE", 2, function_name = "load_data")
        chunksize = abs(as.integer(chunksize))
        if (chunksize < 1)
          .self$log("specify a valid chunksize", 2, function_name = "load_data")
        message("loading chunked data...")
        tryCatch({
          get_chunked_redcap_data(
            api = .self$api,
            token = .self$token,
            local = .self$local,
            chunksize = chunksize,
            forms = NULL,
            fields = NULL,
            ids_to_pull = NULL,
            dataset_name = "records",
            metadataset_name = "meta"
          )
        },
        warning = function(w) {
          .self$log(w, 1, function_name = "load_data")
        },
        error = function(e) {
          .self$log(e, 2, function_name = "load_data")
        })
        .self$.__cache$raw_records = records
        .self$.__cache$raw_meta = meta
        message("records and metadata loaded")
      } else {
        message("loading data in bulk...")
        tryCatch({
          meta = get_redcap_data(
            api = .self$api,
            token = .self$token,
            content = "metadata",
            local = .self$local,
            forms = forms,
            fields = fields,
            ids_to_pull = ids_to_pull
          )
          records = get_redcap_data(
            api = .self$api,
            token = .self$token,
            content = "record",
            local = .self$local
          )
        },
        warning = function(w) {
          .self$log(w, 1, function_name = "load_data")
        },
        error = function(e) {
          .self$log(e, 2, function_name = "load_data")
        })
        .self$.__cache$raw_records = records
        .self$.__cache$raw_meta = meta
        message("records and metadata loaded")
      }
      .self$log("records and metadata loaded", 0, function_name = "load_data")
    },

    load_metadata = function() {
      "Stream metadata only from REDCap if not in cache"

      tryCatch({
        meta = get_redcap_data(
          api = .self$api,
          token = .self$token,
          content = "metadata",
          local = .self$local
        )
      },
      warning = function(w) {
        .self$log(w, 1, function_name = "load_metadata")
      },
      error = function(e) {
        .self$log(e, 2, function_name = "load_metadata")
      })
      .self$.__cache$raw_meta = meta
      .self$log("metadata downloaded", 0, function_name = "load_metadata")
    },

    clean_records = function() {
      "Clean rceords removing out of range and empty values"

      if (!"clean_records" %in% names(as.list(.self$.__cache))) {
        if (!"fmt_records" %in% names(as.list(.self$.__cache))) {
          message("formatted data not in cache, attempting to format raw data...")
          .self$format_records()
        }
        message("cleaning data...")
        dataset = data.frame(.self$.__cache$fmt_records)
        if (!"clean_cmd" %in% names(as.list(.self$.__cache))) {
          .self$.__cache$clean_cmd = paste0(
            generate_remove_missing_code(.self$get_metadata(), "dataset"),
            generate_date_conversion_code(.self$get_metadata(), "dataset"),
            generate_remove_outliers_code(.self$get_metadata(), "dataset"),
            sep = "\n"
          )
        }
        tryCatch({
          eval(parse(text=.self$.__cache$clean_cmd))
        },
        warning = function(w) {
          .self$log(w$message, 1, function_name = "clean_records")
        },
        error = function(e) {
          .self$log(e$message, 2, function_name = "clean_records")
        })
        .self$.__cache$clean_records = dataset
        message("data cleaned")
        .self$log("data cleaned", 0, function_name = "clean_records")
      } else {
        message("data already cleaned")
      }
    },

    clean_meta = function(is_error = FALSE) {
      "Clean meta data for autogeneration of cleaning code"
      if (!"clean_meta" %in% names(as.list(.self$.__cache))) {
        message("cleaning metadata...")
        if (!"raw_meta" %in% names(as.list(.self$.__cache))) {
          message("metadata not in cache, attempting download...")
          .self$load_metadata()
        }
        cln_mt = .self$get_metadata()
        cln_mt = data.table::data.table(cln_mt)
        cln_mt = cln_mt[, key = .I]
        data.table::setkey(cln_mt, key)
        cln_mt = cln_mt[!field_type %in% c("descriptive", "calc")]
        if (is_error) {
          if (!is.null(vars_to_exclude))
            print("not implemented")
        }
        .self$.__cache$clean_meta = cln_mt
        .self$log("metadata cleaned", 0, function_name = "clean_meta")
        message("metadata cleaned")
      } else {
        message("metadata already cleaned")
      }
    },

    format_records = function() {
      "format records to add Hmisc labels and create factors"

      if (!"fmt_records" %in% names(as.list(.self$.__cache))) {
        if (!"raw_records" %in% names(as.list(.self$.__cache))) {
          message("data not in cache, attempting a chunked download (chunksize=500)...")
          .self$load_data(chunked = TRUE, chunksize = 500)
        }
        dataset = .self$.__cache$raw_records
        message("formatting data..")
        if (!"fmt_cmd" %in% names(as.list(.self$.__cache)))
          .self$.__cache$fmt_cmd = generate_formatting_code(.self$get_metadata(), dataset_name = "dataset")
        tryCatch({
          eval(parse(text=.self$.__cache$fmt_cmd))
        }, warning = function(w) {
          .self$log(w$message, 1, function_name = "format_records")
        }, error = function(e) {
          .self$log(e$message, 2, function_name = "format_records")
        })
        .self$.__cache$fmt_records = dataset
        message("formatting done")
        .self$log("data formatted", 0, function_name = "format_records")
      } else {
        message("data already formatted")
      }
    },

    report_errors = function() {
      "Create error report"

      if (!"raw_records" %in% names(as.list(.self$.__cache)))
        .self$get_raw_records()
      dataset = .self$.__cache$raw_records

    },

    get_error_report = function(save_to = NULL, pop = TRUE) {
      "Get error report"

      if (!"error_rpt" %in% names(as.list(.self$.__cache)))
        .self$report_errors()
      errors = .self$.__cache$error_rpt
      if (is.null(save_to))
        save_to = tempfile(pattern = "0000xfjhgggsqiu", fileext = ".R")
      tryCatch({
        write.csv(errors, save_to, row.names = FALSE)
        if (pop)
          open_using_default_app(save_to)
        else
          message(paste0("Error report saved to ", sQuote(save_to)))
      },
      warning = function(w) {
        .self$log(w$message, 1, function_name = "get_error_report")
      },
      error = function(e) {
        .self$log(e$message, 2, function_name = "get_error_report")
      })
      .self$log("error report accessed", 0, function_name = "get_error_report")
    },

    get_raw_data = function() {
      "Get saved records from cache"

      if (!"raw_records" %in% names(as.list(.self$.__cache)))
        stop("no data in memory. use load_data to load cache")
      .self$log("raw data accessed", 0, function_name = "get_raw_data")
      .self$.__cache$raw_records
    },

    get_clean_data = function() {
      "Get saved records from cache"

      if (!"clean_records" %in% names(as.list(.self$.__cache)))
        .self$clean_records()
      .self$log("clean data accessed", 0, function_name = "get_clean_data")
      .self$.__cache$clean_records
    },

    get_formatted_data = function() {
      "Get saved records from cache"

      if (!"fmt_records" %in% names(as.list(.self$.__cache)))
        .self$format_records()
      .self$log("formatted data accessed", 0, function_name = "get_formatted_data")
      .self$.__cache$fmt_records
    },

    get_metadata = function() {
      "Get saved metadata from cache"

      if (!"raw_meta" %in% names(as.list(.self$.__cache)))
        .self$load_metadata()
      .self$log("metadata accessed", 0, function_name = "get_metadata")
      .self$.__cache$raw_meta
    },

    log = function(message, level = 0, function_name = "") {
      "Log events <internal>"

      tmp = if (level == 0) {
        "info"
      } else if (level == 1) {
        "warn"
      } else if (level == 2) {
        "error"
      } else {
        stop("not implemented!")
      }
      timestamp = format(Sys.time(), "%Y-%m-%d [%I:%M%p]")
      tolog = "\n"
      if (length(.self$.__log) == 0) {
        tolog = paste0("Timestamp", paste0(rep("\t", 15), collapse = ""),
                       "Level", paste0(rep("\t", 10), collapse = ""), "Message\n")
      }
      tolog = paste0(tolog,
                     timestamp, "\t\t***", tmp, "***\t\t", paste0("{", function_name, "} ", message)
      )
      .__log <<- paste0(.__log, tolog)
      if (level == 1)
        warning(sQuote(message), call. = FALSE)
      if (level == 2)
        stop(sQuote(message), call. = FALSE)
    }
  )
)

#' @name redcap
#'
#' @title Wrapper for creating REDCap objects
#'
#' @description Cleaner way for instantiating REDCap objects.
#'
#' Avoids messing up instantiation in R6 classes which is more complex and error prone.
#'
#' @details This function acts as a wrapper for instantiating an object that abstracts the REDCap API.
#'
#' It performs the necessary checks for instantiation hence avoids messy objects.
#'
#' It then calls the new method of the underlying reference class.
#'
#' @param api_url REDCap project's api url. Just append /api/ to the project's url.
#' @param token The secret token for the project. Check the API page in REDCap. Must have api rights to access this.
#' @param local Whether REDCap instance is local.
#' @param exclusion_pattern regex for variables to exclude from autogeneration of code
#' @param custom_code Custom error report code
#'
#' @export
#'
#' @seealso \code{\link{redcap_class}}
#'
#' @include redcap_update.R
#'
#' @return A redcap class instance that can be used to interact with the data repository
#'
#' @examples \dontrun{cin = redcap("<some-token>", api_url = "http://<some-dns>/redcap/api/", local = F)}
#'
#' \dontrun{cin}


redcap = function(
  token,
  api_url = "http://localhost/redcap/api/",
  local = TRUE,
  exclusion_pattern = NA_character_,
  custom_error_file = NA_character_
) {
  if (is.na(local))
    stop("specify whether redcap is local instance")
  if (!is.logical(local))
    stop("invalid local")
  if (length(local) == 0L)
    local = TRUE
  if (length(local) > 1L) {
    warning("local is of length > 1, taking first element")
    local = local[1L]
  }
  tmp = "http://localhost/redcap/api/"
  if (local) {
    if (!missing(api_url) & api_url != tmp)
      message("local=T, resetting api_url=", tmp)
    api_url = tmp
  } else {
    if (missing(api_url))
      stop("specify api url")
    if (stringr::str_trim(api_url) == "" | is.na(api_url))
      stop("specifiy a valid api url")
    if (api_url==tmp) {
      message("api_url=", tmp ,", resetting local=TRUE")
      local = TRUE
    }
    if (!grepl("/api/$", api_url)) {
      stop("api_url invalid. ???<must end with \"/api/\">???")
    }
  }
  if (missing(token))
    stop("specify token")
  if (stringr::str_trim(token) == "" | is.na(token))
    stop("specifiy a valid api token")
  if (!is.na(custom_error_file)) {
    if (!file.exists(custom_error_file))
      stop("custom code not found")
    custom_error_file = readLines(custom_error_file, warn = FALSE)
    custom_error_file = cat(custom_error_file, collapse = "\n")
  }
  if (!is.na(exclusion_pattern))
    if (!is.character(exclusion_pattern))
      stop("invalid exclusion pattern")
  redcap_class$new(api = api_url, token = token, local = local, exclusion_pattern = exclusion_pattern, custom_code = custom_error_file)
}
