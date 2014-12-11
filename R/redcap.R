#' @include redcap_helper.R
NULL

#' @include redcap_config.R
NULL

#' @include redcap_config_helper.R
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
#' It uses a cache system and logs major events.
#'
#' Show the object to view status.
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
#' @section Info:
#' Use redcap function to instantiate this class. This avoids many pitfalls during the lifetime of this object.
#'
#' @include redcap_config.R
#'

redcap_class = setRefClass(
  "Redcap",
  fields = list(
    .__cache = "environment",
    .__log = "character",
    opts = "RedcapConfig"
  ),

  methods = list(

    show = function() {
      www = gsub("/api/", "", .self$opts$configs$api_url)
      if (.self$opts$configs$api_url)
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

    load_data = function(chunked = .self$opts$configs$chunked,
                         chunksize = .self$opts$configs$chunksize
    ) {
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
            api = .self$opts$configs$api_url,
            token = .self$opts$configs$token,
            local = .self$opts$configs$local,
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
            api = .self$opts$configs$api_url,
            token = .self$opts$configs$token,
            content = "metadata",
            local = .self$opts$configs$local
          )
          records = get_redcap_data(
            api = .self$opts$configs$api_url,
            token = .self$opts$configs$token,
            content = "record",
            local = .self$opts$configs$local
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
          api = .self$opts$configs$api_url,
          token = .self$opts$configs$token,
          content = "metadata",
          local = .self$opts$configs$local
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

      if (!"clean_records" %in% ls(all = T, envir = .self$.__cache)) {
        if (!"fmt_records" %in% ls(all = T, envir = .self$.__cache)) {
          message("formatted data not in cache, attempting to format raw data...")
          .self$format_records()
        }
        message("cleaning data...")
        dataset = data.frame(.self$.__cache$fmt_records)
        if (!"clean_cmd" %in% ls(all = T, envir = .self$.__cache)) {
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

    get_clean_meta = function(is_error = FALSE) {
      "Clean meta data for autogeneration of error report code"

      if (!"clean_meta" %in% ls(all = T, envir = .self$.__cache)) {
        message("cleaning metadata...")
        cln_mt = .self$get_metadata()
        cln_mt = data.table(cln_mt)
        cln_mt = cln_mt[, key = .I]
        setkey(cln_mt, key)
        cln_mt = cln_mt[!field_type %in% c("descriptive", "calc")]
        if (is_error) {
          if (!is.na(vars_to_exclude)) {
            to_exclude = sapply(.self$opts$configs$exclusion_pattern, function(pt) {
              grepl(pt, cln_mt[, field_name])
            })
            to_exclude = apply(to_exclude, 1, any)
            cln_mt = cln_mt[!to_exclude]
          }
        }
        .self$.__cache$clean_meta = data.frame(cln_mt)
        .self$log("metadata cleaned", 0, function_name = "get_clean_meta")
        message("metadata cleaned")
      }
      .self$log("clean metadata accessed", 0, function_name = "get_clean_meta")
      .self$.__cache$clean_meta
    },

    format_records = function() {
      "format records to add Hmisc labels and create factors"

      if (!"fmt_records" %in% ls(all = T, envir = .self$.__cache)) {
        dataset = .self$get_raw_data()
        message("formatting data..")
        if (!"fmt_cmd" %in% ls(all = T, envir = .self$.__cache))
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

      dataset = .self$get_raw_data()
      if (!"data.table" %in% class(dataset))
        dataset = data.table(dataset)
      if (!haskey(dataset)) {
        dataset = dataset[, key_x2014cin := .I]
        setkey(dataset, key_x2014cin)
      }
      if (!"err_cmd" %in% ls(all.names = T, envir = .self$.__cache))
        .self$.__cache$err_cmd = generate_error_report_code(
          .self$get_clean_metadata(),
          dataset_name = "dataset",
          date_var = .self$opts$configs$date_var,
          hosp_var = .self$opts$configs$hosp_var,
          custom_code = .self$opts$configs$custom_code,
          updates = .self$opts$updates
        )
      eval(parse(text = .self$.__cache))
      .self$.__cache$err_rpt = dataset[, validate_data_entry(.SD), by = key_x2014cin]
    },

    get_error_report = function(save_to = NULL, pop = TRUE) {
      "Get error report"

      if (!"err_rpt" %in% ls(all = T, envir = .self$.__cache))
        .self$report_errors()
      errors = .self$.__cache$err_rpt
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

      if (!"raw_records" %in% ls(all = T, envir = .self$.__cache))
        stop("no data in memory. use load_data to load cache")
      .self$log("raw data accessed", 0, function_name = "get_raw_data")
      .self$.__cache$raw_records
    },

    get_clean_data = function() {
      "Get saved records from cache"

      if (!"clean_records" %in% ls(all = T, envir = .self$.__cache))
        .self$clean_records()
      .self$log("clean data accessed", 0, function_name = "get_clean_data")
      .self$.__cache$clean_records
    },

    get_formatted_data = function() {
      "Get saved records from cache"

      if (!"fmt_records" %in% ls(all = T, envir = .self$.__cache))
        .self$format_records()
      .self$log("formatted data accessed", 0, function_name = "get_formatted_data")
      .self$.__cache$fmt_records
    },

    get_metadata = function() {
      "Get saved metadata from cache"

      if (!"raw_meta" %in% ls(all = T, envir = .self$.__cache))
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
#' @return A redcap class instance that can be used to interact with the data repository
#'
#' @examples \dontrun{cin = redcap("<some-token>", api_url = "http://<some-dns>/redcap/api/", local = F)}
#'
#' \dontrun{cin}


redcap = function(
  configs_location = NA,
  custom_code_location = NA,
  updates_location = NA,
  exclusion_pattern = NA
) {
  opts = list()
  if (!is.na(configs_file_location)) {
    if (!file.exists(configs_file_location))
      stop("configs file not found")
    configs_data = read.csv(configs_file_location, as.is = TRUE)
  } else {
    configs_data = NULL
  }
  opts$config_data = configs_data
  if (!is.na(custom_code_location)) {
    if (!file.exists(custom_code_location))
      stop("custom code file not found")
    custom_code = readLines(custom_code_location, warn = F)
  } else {
    custom_code = NA
  }
  opts$custom_code = custom_code

  if (!is.na(exclusion_pattern))
    if (!is.character(exclusion_pattern))
      stop("invalid exclusion pattern")
  opts$exclusion_pattern = exclusion_pattern
  tryCatch({ configs = do.call(load_configs, opts) },
           warning = function(w) warning(w$message),
           error = function(e) stop("Could not load configs\nDetails:\n", e$message)
           )
  configs = do.call(load_configs, opts)
  if (!is.na(updates_location)) {
    if (!file.exists(updates_location))
      stop("updates file not found")
    updates = read.csv(updates_location, as.is = TRUE)
    tryCatch({ updates = load_updates(updates) }, warning = function(w) warning(w$message), error = function(e) updates = list())
  } else {
    updates = list()
  }
  configs$updates <<- updates
  if (!is.valid(configs))
    stop("invalid configs")
  obj = redcap_class$new(opts = configs)
  obj
}
