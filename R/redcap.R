#' @include redcap_helper.R
NULL

#' @include redcap_config.R
NULL

#' @include redcap_config_helper.R
NULL

#' @name Redcap
#'
#' @title Redcap Wrapper Class
#'
#' @aliases redcap_wrapper redcap_class
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
#' @field opts RedcapConfig object that controls the mode of interaction with the data repoistory during this object's lifecycle.
#'
#' @return A redcap class with an array of data-allied functionality.
#'
#' @seealso \code{\link{redcap_project}}
#'
#' @include redcap_config.R
#'
#'
#'

Redcap = setRefClass(
  "Redcap",
  fields = list(
    .__cache = "environment",
    .__log = "character",
    opts = "RedcapConfig"
  ),

  methods = list(

    show = function() {
      www = gsub("/api/", "", .self$opts$configs$api_url)
      if (.self$opts$configs$local)
        msg = "redcap:\nA local redcap instance <class:Redcap>\n"
      else
        msg = paste0("redcap:\nA remote redcap instance running at ", sQuote(www), " <class:Redcap>\n")
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

    load_data = function() {
      "Stream data from REDCap.
      Clears the cache and loads data.
      < NOTE: Do this only for initial loading or when you are sure there are changes in the data repo >.
      "

      if (length(ls(.self$.__cache)))
        message("NOTE: cache has been cleared")
      rm(list = ls(.self$.__cache), envir = .self$.__cache)
      if (.self$opts$chunked) {
        if (!is.numeric(.self$opts$chunksize))
          .self$log("chunksize not specified yet chunked=TRUE", 2, function_name = "load_data")
        if (.self$opts$chunksize < 1)
          .self$log("specify a valid chunksize", 2, function_name = "load_data")
        message("loading chunked data...")
        tryCatch({
          get_chunked_redcap_data(
            api = .self$opts$configs$api_url,
            token = .self$opts$configs$token,
            local = .self$opts$configs$local,
            chunksize = .self$opts$chunksize,
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
      "Stream metadata only from REDCap"

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
      "Clean records removing out of range and coded missing values"

      if (!"clean_records" %in% ls(all = T, envir = .self$.__cache)) {
        if (!"fmt_records" %in% ls(all = T, envir = .self$.__cache)) {
          message("formatted data not in cache, attempting to format raw data...")
          .self$format_records()
        }
        message("cleaning data...")
        dataset = data.frame(.self$.__cache$fmt_records)
        if (!"clean_cmd" %in% ls(all = T, envir = .self$.__cache)) {
          .self$.__cache$clean_cmd = paste0(
            "\n# <Note: !! Do not edit this code as it may change in future code regenerations. !!>",
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

    get_clean_metadata = function() {
      "Clean meta data for autogeneration of error report code"

      if (!"clean_meta" %in% ls(all = T, envir = .self$.__cache)) {
        message("cleaning metadata...")
        cln_mt = .self$get_metadata()
        cln_mt = data.table(cln_mt)
        cln_mt = cln_mt[, key := .I]
        setkey(cln_mt, key)
        cln_mt = cln_mt[field_type != "descriptive"]
        cln_mt = cln_mt[field_type == "checkbox", required_field := "y"]

        if (length(na.omit(.self$opts$configs$exclusion_pattern)) != 0) {
          to_exclude = as.character(.self$opts$configs$exclusion_pattern)
          to_exclude = sapply(to_exclude, function(pt) {
            grepl(pt, cln_mt[, field_name])
          })
          to_exclude = apply(to_exclude, 1, any, na.rm = TRUE)
          cln_mt = cln_mt[!to_exclude]
        }
        .self$.__cache$clean_meta = data.frame(cln_mt)
        .self$log("metadata cleaned", 0, function_name = "get_clean_metadata")
        message("metadata cleaned")
      }
      .self$log("clean metadata accessed", 0, function_name = "get_clean_metadata")
      .self$.__cache$clean_meta
    },

    format_records = function() {
      "Format records to add Hmisc labels and create factors"

      if (!"fmt_records" %in% ls(all = T, envir = .self$.__cache)) {
        dataset = .self$get_raw_data()
        message("formatting data...")
        if (!"fmt_cmd" %in% ls(all = T, envir = .self$.__cache))
          .self$.__cache$fmt_cmd = paste0(
            "\n# <Note: !! Do not edit this code as it may change in future code regenerations. !!>",
            generate_formatting_code(.self$get_metadata(), dataset_name = "dataset"), sep = "\n")
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
      upds = .self$opts$updates
      if (length(upds) == 0)
        upds = NULL
      message("generating error report code...")
      tryCatch({
        if (!"validate_data_entry" %in% ls(all.names = T, envir = .self$.__cache)) {
          tmp = generate_error_report_code(
            .self$get_clean_metadata(),
            date_var = .self$opts$configs$date_var,
            hosp_var = .self$opts$configs$hosp_var,
            custom_code = .self$opts$configs$custom_code,
            updates = "upds",
            updates_envir_depth = 2
          )
          message("error report code generated")
          .self$log("error report code generated", 0, function_name = "report_errors")
          eval(parse(text = tmp), envir = .self$.__cache)
          .self$log("error report function in memory", 0, function_name = "report_errors")
        }
        message("generating report. This might take a while...")
        if (.self$opts$configs$chunked) {
          .counter = .self$opts$configs$chunksize
          rpt = lapply(get_chunks(1 : nrow(dataset), .self$opts$configs$chunksize), function(chunk) {
            ds_chunk = dataset[chunk, .__cache$validate_data_entry(.SD), by = key_x2014cin]
            message(paste0("validated ", min(100, round((.counter * 100) / nrow(dataset), 2)), "%", ifelse(.counter >= nrow(dataset), "", "...")))
            assign(".counter", (.counter + .self$opts$configs$chunksize), envir = parent.frame(2))
            ds_chunk
          })
          rpt = data.table::rbindlist(rpt)
        } else {
          rpt = dataset[, .__cache$validate_data_entry(.SD), by = key_x2014cin]
        }
        rpt = rpt[, key_x2014cin := NULL]
        if (nrow(rpt) == 0) {
          rpt = data.table(Message == "No errors in data capture!")
        }
        message("report generated")
        .self$log("error report created", 0, function_name = "report_errors")
      }, warning = function(w) {
        .self$log(w$message, 1, function_name = "report_errors")
        generate_error_report_code(
          .self$get_clean_metadata(),
          date_var = .self$opts$configs$date_var,
          hosp_var = .self$opts$configs$hosp_var,
          custom_code = .self$opts$configs$custom_code,
          updates = upds
        )
      }, error = function(e) {
        .self$log(e$message, 2, function_name = "report_errors")
      })
      .self$.__cache$err_rpt = rpt
    },

    get_error_report = function(pop = TRUE) {
      "Get error report"

      if (!"err_rpt" %in% ls(all = T, envir = .self$.__cache))
        .self$report_errors()
      errors = .self$.__cache$err_rpt
      tryCatch({
        tmp = gsub("\\\\", "/", .self$opts$configs$report_location)
        tmpdir = unlist(strsplit(tmp, "/"))
        tmpdir = paste0(tmpdir[-length(tmpdir)], collapse = "/")
        if (!file.exists(tmpdir))
          dir.create(tmpdir, recursive = TRUE)
        write.csv(errors, .self$opts$configs$report_location, row.names = FALSE)
        if (pop)
          open_using_default_app(.self$opts$configs$report_location)
        else
          message(paste0("Error report saved to ", sQuote(.self$opts$configs$report_location)))
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
      "Get raw records from memory.
      If there is not data in memory, an error is raised.
      Use load_data() to refresh the cache.
      "

      if (!"raw_records" %in% ls(all = T, envir = .self$.__cache))
        stop("no data in memory. use load_data to load cache")
      .self$log("raw data accessed", 0, function_name = "get_raw_data")
      .self$.__cache$raw_records
    },

    get_clean_data = function() {
      "Get cleaned records from memory"

      if (!"clean_records" %in% ls(all = T, envir = .self$.__cache))
        .self$clean_records()
      .self$log("clean data accessed", 0, function_name = "get_clean_data")
      .self$.__cache$clean_records
    },

    get_formatted_data = function() {
      "Get formatted records from memory"

      if (!"fmt_records" %in% ls(all = T, envir = .self$.__cache))
        .self$format_records()
      .self$log("formatted data accessed", 0, function_name = "get_formatted_data")
      .self$.__cache$fmt_records
    },

    get_metadata = function() {
      "Get raw metadata from memory"

      if (!"raw_meta" %in% ls(all = T, envir = .self$.__cache))
        .self$load_metadata()
      .self$log("metadata accessed", 0, function_name = "get_metadata")
      .self$.__cache$raw_meta
    },

    log = function(message, level = 0, function_name = "") {
      "Log events <internal use>"

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
                     timestamp, "\t\t***", tmp, "***\t\t", paste0("[fun: ", function_name, "] ", message)
      )
      .__log <<- paste0(.__log, tolog)
      if (level == 1)
        warning(sQuote(message), call. = FALSE)
      if (level == 2)
        stop(sQuote(message), call. = FALSE)
    }
  )
)

#' @rdname RedcapProject
#'
#' @name redcap_project
#'
#' @title Wrapper for creating REDCap objects
#'
#' @aliases cin_project
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
#' The configs and update files must be csv files. See \code{\link{load_configs}}, \code{\link{load_updates}} for details.
#'
#' The custom code must hold valid R code.
#'
#' The exclusion patterns must hold valid R regex expressions. Can be full variable names or a set of patterns.
#'
#' @param configs_location Location of the configs file (csv). See details...
#' @param custom_code_location Location of any custom code for error reporting (.R | .txt). See details...
#' @param updates_location Location of a file containing any updates to redcap metadata. (csv) See details...
#' @param exclusion_pattern A character vector of regex patterns for variables to exclude from autogeneration of error reporting code.
#'
#' @export
#'
#' @seealso \code{\link{Redcap}}
#'
#' @return A redcap class instance that can be used to interact with the data repository
#'

redcap_project = function(
  configs_location,
  custom_code_location = NA,
  updates_location = NA,
  exclusion_pattern = NA_character_
) {
  opts = list()
  if (is.na(configs_location))
    stop("Specify configuration file location")
  if (!file.exists(configs_location))
    stop("configurations file not found")
  configs_data = read.csv(configs_location, as.is = TRUE)
  if (!all(c("key", "value", "type") %in% names(configs_data)))
    stop("invalid configurations data [must have `key`, `value` and `type` entries. See help details.")
  configs_data = configs_data[, c("key", "value", "type")]
  opts$config_data = configs_data
  if (!is.na(custom_code_location)) {
    if (!file.exists(custom_code_location))
      stop("custom code file not found")
    custom_code = readLines(custom_code_location, warn = F)
  } else {
    custom_code = NA_character_
  }
  opts$custom_code = custom_code
  exclusion_pattern = as.character(na.omit(exclusion_pattern))
  if (length(exclusion_pattern) == 0)
    exclusion_pattern = NA_character_
  opts$exclusion_pattern = exclusion_pattern
  configs = do.call(load_configs, opts)
  if (!is.na(updates_location)) {
    if (!file.exists(updates_location))
      stop("updates file not found")
    updates = read.csv(updates_location, as.is = TRUE)
    updates = load_updates(updates)
  } else {
    updates = list()
  }
  configs$updates = updates
  if (!configs$is_valid())
    stop("invalid configs")
  obj = Redcap$new(opts = configs)
  obj
}
