#' @name get_chunks
#'
#' @include generic_helper.R
#'
#' @title Get chunks of a specific size
#'
#'
#' @description From a vector obtain a list of chunks of a specific size
#'
#' @return A list of objects of the same data type as input, each of size equal to the chunk size.
#'
#' The last chunk may be smaller if the input's length is not the LCM of the input's length and the chunks size.
#'
#' @details This is a utility functiion that facilitates chunked operations.
#'
#' A common scenario is to chunk the indices of a specific object and then perform operations on the chunks separately and then later merge the output.
#'
#' This can be useful in situations where multiple operations of a smaller scope is better than bulk processing.
#'
#' @param x Input
#' @param chunksize The size of the chunks
#'
#' @seealso \code{\link{get_chunked_redcap_data}}
#'
#' @export
#'
#'

get_chunks = function(x, chunksize) {
  n = length(x)
  if (n < 1)
    stop("specify input")
  if (n <= chunksize)
    return(list(x))
  n_grp = ceiling(n / chunksize)
  value = list()
  start = 1
  end = chunksize
  for (i in 1:n_grp) {
    value[[i]] = x[seq(start, end)]
    start = start + chunksize
    end = end + chunksize
    if (end > n)
      end = n
  }
  value
}

#' @name get_chunked_redcap_data
#'
#' @title Perform a chunked REDCap data dowmload
#'
#' @description Download data in chunks from a REDCap repository using the REDCap api.
#'
#' If not specified, data and metadata are stored in the current environment.
#'
#' @details The record identifiers are chunked and then data is is pulled for those specific records.
#'
#' The result is then combined into one data table.
#'
#' This might be handy in the case of network latency or when the data is large.
#'
#' This is just a convenient wrapper round the \code{\link{get_redcap_data}} function.
#'
#' @param api the REDCap instance's api location. Just append /api/ to the instance's url.
#' @param token The secret token for the project. Check the API page in REDCap. Must have api rights to access this.
#' @param local Whether the REDCap instance is local.
#' @param chunksize The size of the chunks to be pulled at a time.
#' @param forms a character vector of the list of form data to pull.
#' @param fields a character vector of the specific fields to pull.
#' @param ids_to_pull a character vector of the specific record itentifiers to pull.
#' @param dataset_name name of the resultant dataset
#' @param metdataset_name name of the resultant metadata
#' @seealso \code{\link{get_chunks}}
#' @export
#'#'
#' @family get_data_red

get_chunked_redcap_data = function(
  api,
  token,
  local = TRUE,
  chunksize = 100,
  forms = NULL,
  fields = NULL,
  ids_to_pull = NULL,
  dataset_name = "records",
  metadataset_name = "meta"
) {
  if (missing(api))
    stop("specify api url")
  if (missing(token))
    stop("specifiy token")
  if (is.na(chunksize))
    stop("chunksize missing")
  if (!is.numeric(chunksize))
    stop("chunksize not numeric")
  chunksize = abs(as.integer(chunksize))
  if (chunksize < 1)
    stop("invalid chunksize")

  outer_env = parent.frame(1)
  if (!exists(metadataset_name, envir = outer_env))
    assign(metadataset_name, get_redcap_data(api, token, local, content = "metadata"), outer_env)
  id_name = get(metadataset_name, envir = outer_env)[1, 1]

  ids_specified = FALSE
  if (!is.null(ids_to_pull)) {
    if (!is.na(ids_to_pull)) {
      ids_list = as.character(unlist(unclass(ids_to_pull)))
      ids_specified = TRUE
    }
  }
  if (!ids_specified) {
    ids_list = as.character(unlist(get_redcap_data(api, token, fields = id_name)))
  }

  data_size = length(ids_list)
  ids_list = get_chunks(ids_list, chunksize)

  tryCatch({
    message(paste0("downloading data from redcap... (", data_size, " rows!)"))
    counter = chunksize
    data_list = lapply(ids_list, function(ids) {
      ds_chunk = get_redcap_data(api = api, token = token, local = local, fields = fields, forms = forms, ids_to_pull = ids)
      message(paste0("downloaded ", min(100, round(counter * 100 / data_size, 2)), "%"))
      assign("counter", counter + chunksize, envir = parent.env(environment()))
      ds_chunk
    })
    assign(dataset_name, data.frame(data.table::rbindlist(data_list)), envir = outer_env)
  },
  error = function(e) {
    stop("chunkdownload - ", sQuote(e$message))
  },
  warning = function(w) {
    warning("chunkdownload - ", sQuote(w$message))
  })
}

#' @name get_redcap_data
#'
#' @title Pull data from REDCap
#'
#' @description Download data from a REDCap repository using the REDCap api in bulk.
#'
#' @return A data frame with the REDCap repo's data.
#'
#' @details A simulation of the posting of a form to the api is done and a connection is obtained.
#'
#' The connection is then used to read the data into REDCap.
#'
#' This is done in bulk. In the case  of network latency or big data \code{\link{get_chunked_redcap_data}} would be a good alternative.
#'
#' @param api the REDCap instance's api location. Just append /api/ to the instance's url.
#' @param token The secret token for the project. Check the API page in REDCap. Must have api rights to access this.
#' @param content What to pull. Currently only "record" and "metadata" are supported.
#' @param local Whether the REDCap instance is local.
#' @param chunksize The size of the chunks to be pulled at a time.
#' @param forms a character vector of the list of form data to pull.
#' @param fields a character vector of the specific fields to pull.
#' @param ids_to_pull a character vector of the specific record itentifiers to pull.
#' @seealso \code{\link{get_chunked_redcap_data}}, \code{\link{redcap}}
#'
#' @export
#'
#' @family get_data_red

get_redcap_data = function(
  api,
  token,
  content = "record",
  local = TRUE,
  forms = NULL,
  fields = NULL,
  ids_to_pull = NULL
) {
  fun_env = environment()
  if (!RCurl::url.exists(gsub("/api/", "", api)))
    stop("invalid api url")
  opts = list(
    uri = api,
    token = token,
    type = "flat",
    format = "csv",
    content = content,
    rawOrLabel = "raw",
    .opts = RCurl::curlOptions(ssl.verifypeer = !local)
  )
  if (!is.null(forms))
    opts$forms = paste0(forms, collapse = ",")
  if (!is.null(fields))
    opts$fields = paste0(fields, collapse = ",")
  if (!is.null(ids_to_pull))
    opts$records = paste0(ids_to_pull, collapse = ",")
  tryCatch({
    redcap_conn = do.call(RCurl::postForm, opts)
  },
  error = function(e) {
    assign("redcap__err", e$message, envir = fun_env)
  },
  warning = function(w) {
    warning(e$message)
  })
  if (exists("redcap__err", envir = fun_env)) {
    msg = sQuote(get("redcap__err", envir = fun_env))
    rm("redcap__err", envir = fun_env)
    stop(paste0("data could not be downloaded [details: ", msg, "]"))
  } else {
    value = data.frame(read.csv(textConnection(redcap_conn), stringsAsFactors = FALSE))
  }
  value
}

#' @name is_valid_metadata
#'
#' @title Check whether metadata is valid
#'
#' @description This performs a check to validate the REDCap metadata provided.
#'
#' @details This makes sure that the REDCap metadata conforms to the expectations of this project.
#'
#' This helps avoid breaking of code in instances where metadata is required as an iput
#'
#' @param metadata REDCap metadata
#'
#' @return TRUE if valid else FALSE
#'

is_valid_metadata = function(metadata) {
  if (!is.data.frame(metadata))
    return(FALSE)
  value = FALSE
  if (all(c(
    "field_name",
    "form_name",
    "section_header",
    "field_label",
    "field_type",
    "text_validation_type_or_show_slider_number",
    "text_validation_min",
    "text_validation_max",
    "select_choices_or_calculations",
    "branching_logic",
    "required_field"
  ) %in% names(metadata)))
  value = TRUE
  value
}

#' @name get_vars_in_data
#'
#' @title Get the names of variables in dataset
#'
#' @description Utility function that gets the name(s) of the variables in a redcap project.
#'
#' @details Using the redcap metadata, code is generated that extracts the names of the dataset.
#'
#' Checkboxes are also munged to reflect what is in the repository.
#'
#' @param metadata REDCap metadata
#'
#' @export
#'
#' @return a character vector of the names of the dataset

get_vars_in_data = function(metadata) {
  metadata = prepare_metadata_for_code_generation(metadata)
  get_vars_r = function(r) {
    var = r$field_name
    widget = r$field_type
    if (widget == "checkbox") {
      choices = r$select_choices_or_calculations
      choices = str_trim(unlist(strsplit(choices, "\\|")))
      choices = sapply(choices, function(ch) {
        lev = str_trim(unlist(strsplit(ch, ",")))[1L]
        lev = gsub("\\-", "\\.", lev)
        lev
      })
      value = data.table(var = paste0(var, "___", choices))
    } else if (widget == "descripive") {
      value = data.table()
    } else {
      value = data.table(var = var)
    }
    value
  }

  value = na.omit(metadata[, get_vars_r(.SD), by = key][, var])
  value
}

#' @name get_r_types_in_data
#'
#' @title Get the r data types of variables in dataset
#'
#' @description Utility function that gets the data type(s) of the variables in a redcap project
#'
#' @details Using the redcap metadata, code is generated that extracts the data types of the variables in the dataset.
#'
#' @param metadata REDCap metadata
#'
#' @return The data types (r) of the variables in the dataset
#'
#'

get_r_types_in_data = function(metadata) {
  metadata = data.table(metadata)
  metadata = metadata[, key := .I]
  if (!is_valid_metadata(metadata))
    stop("metadata not valid")

  get_r_type_r = function(r) {
    widget = r$field_type
    if (widget == "checkbox") {
      choices = r$select_choices_or_calculations
      choices = sapply(strsplit(choices, "\\|"), function(ch) {
        lev = str_trim(unlist(strsplit(ch, ",")))[1L]
        lev = gsub("\\-", "\\.", lev)
        lev
      })
      value = paste0(widget, "___", choices)
    } else if (widget == "descripive") {
      value = NA
    } else {
      value = widget
    }
    value
  }

  value = na.omit(metadata[, get_vars_r(.SD), by = key][, V1])
  value
}

#' @name generate_remove_missing_code
#'
#' @title Autogenerate code for cleaning data
#'
#' @description This is a utility function that employs code generation to produce r code for cleaning data.
#'
#' @details Using the redcap metadata, code is generated that removes missing data from repo.
#'
#' This is useful as missing data is coded and has to be recoded as NA.
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded
#'
#' @export
#'
#' @return Code that can be evaluated to clean data
#'
#' @family code_gen

generate_remove_missing_code = function(metadata, dataset_name = "data") {
  metadata = prepare_metadata_for_code_generation(metadata)
  invalid_vals = c(
    "as.character(seq(as.Date(\"1910-01-01\"), as.Date(\"1950-01-01\"), by = \"year\"))",
    "\"-1\"", "\"Empty\"", , "\"empty\"", "\"\""
  )
  invalid_vals = paste0(invalid_vals, collapse = ", ")
  invalid_vals = paste0("c(", invalid_vals, ")")

  cmd = str_trim(get_vars_in_data(metadata))
  if (length(cmd) == 0L)
    return("")
  cmd = paste0(dataset_name, "$", cmd, "[str_trim(", dataset_name, "$", cmd, ") %in% ", invalid_vals, "] = NA")
  cmd = c(
    "\n\n# RECODING MISSING DATA TO NA\n",
    "library(stringr)",
    paste0(dataset_name, " = data.frame(", dataset_name, ")"),
    cmd
  )
  cmd = paste0(cmd, collapse = "\n")
  cmd
}

#' @name generate_remove_outliers_code
#'
#' @title Autogenerate code for cleaning data
#'
#' @description This is a utility function that employs code generation to produce r code for cleaning data.
#'
#' @details Using the redcap metadata, code is generated that removes out-of-range data from repo.
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded
#'
#' @export
#'
#' @return Code that can be evaluated to clean data
#'
#' @family code_gen

generate_remove_outliers_code = function(metadata, dataset_name = "data") {
  metadata = prepare_metadata_for_code_generation(metadata)
  check_miss = function(s) {
    is.na(s) | str_trim(s) == ""
  }
  has_min = !sapply(metadata$text_validation_min, check_miss)
  has_max = !sapply(metadata$text_validation_max, check_miss)
  has_valid = which(apply(cbind(has_min, has_max), 1, any, na.rm = TRUE))
  if (length(has_valid) == 0L)
    return("")
  metadata = metadata[has_valid]

  generate_code_r = function(r) {
    cmd = character(0L)
    var_r = str_trim(r$field_name)
    type_r = str_trim(r$text_validation_type_or_show_slider_number)
    min_r = str_trim(r$text_validation_min)
    max_r = str_trim(r$text_validation_max)
    if (type_r == "integer") {
      type = "int"
      na_val = "NA_integer_"
    } else if (type_r == "number") {
      type = "num"
      na_val = "NA_real_"
    } else if (type_r == "date_ymd") {
      type = "date"
      na_val = "as.Date(NA)"
    } else {
      type = "char"
      na_val = "NA_character"
    }
    suffix = if (type_r == "integer")
      "L"
    else
      ""
    has_min_r = all(!is.na(min_r), str_trim(min_r) != "")
    has_max_r = all(!is.na(max_r), str_trim(max_r) != "")

    if (any(has_min_r, has_max_r)) {
      if (has_min_r) {
        tmp = min_r
        if (type == "date")
          tmp = paste0("as.Date(\"", tmp, "\")")
        cmd = paste0(cmd, dataset_name, "$", var_r, "[", dataset_name, "$", var_r, " < ", tmp, suffix)
      }
      if (has_max_r) {
        tmp = max_r
        if (type == "date")
          tmp = paste0("as.Date(\"", tmp, "\")")
        if (!has_min_r)
          cmd = paste0(cmd, dataset_name, "$", var_r, "[", dataset_name, "$", var_r, " > ", tmp, suffix)
        else
          cmd = paste0(cmd, " | ", dataset_name, "$", var_r, " > ", tmp, suffix)
      }
      cmd = paste0(cmd, "] = NA")
    }
    cmd
  }
  cmd = metadata[, generate_code_r(.SD), by = key]
  cmd = paste0(cmd[, V1], collapse = "\n")
  cmd = paste0("\n\n#RECODING OUT OF RANGE VALUES TO NA\n\n",
               paste0(dataset_name, " = data.frame(", dataset_name, ")\n"),
               cmd
  )
  cmd
}

#' @name generate_date_conversion_code
#'
#' @title Autogenerate code for date conversion
#'
#' @description This is a utility function that employs code generation to produce r code that recodes data.
#'
#' @details Using the redcap metadata, code is generated that converts character dates to R Date variables.
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded
#'
#' @export
#'
#' @return Code that can be evaluated to recode dates
#'
#' @family code_gen

generate_date_conversion_code = function(metadata, dataset_name = "data") {
  metadata = prepare_metadata_for_code_generation(metadata)
  metadata = metadata[str_trim(text_validation_type_or_show_slider_number) == "date_ymd"]
  if (nrow(metadata) < 1L)
    return("")
  cmd = get_vars_in_data(metadata)
  cmd = paste0(dataset_name, "$", cmd, " = as.Date(", dataset_name, "$", cmd, ")")
  cmd = paste0(cmd, collapse = "\n")
  cmd = c("\n\n#DATE CONVERSION\n",
          paste0(dataset_name, " = data.frame(", dataset_name, ")"),
          cmd
  )
  cmd = paste(cmd, collapse = "\n")
  cmd
}

#' @name generate_formatting_code
#'
#' @title Autogenerate code for data formatting
#'
#' @description This is a utility function that employs code generation to produce r code that formats data.
#'
#' @details Using the redcap metadata, code is generated that formats data.
#'
#' It assigns HMisc labels to variables and recodes data to factors
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded
#'
#' @export
#'
#' @return Code that can be evaluated to format data
#'
#' @family code_gen
#'
#' @include branching_logic.R

generate_formatting_code = function(metadata, dataset_name = "data") {
  metadata = prepare_metadata_for_code_generation(metadata)
  to_remove = paste0(unique(metadata[, form_name]),"_complete")
  metadata = metadata[!field_name %in% to_remove]
  reshape_labels = function(x) {
    if (tolower(x[, field_type]) %in% c("checkbox", "dropdown", "radio")) {
      choices =  t(
        sapply(
          unlist(strsplit(x[, select_choices_or_calculations], "\\|")),
          function(ch) {
            ch_ls = str_trim(unlist(regmatches(ch, regexpr(",", ch), invert = TRUE)))
            names(ch_ls) = c("level", "label")
            ch_ls
          }))
      if (x[, field_type] == "checkbox") {
        variable = paste0(x[, field_name], "___", choices[, 1])
        label = paste0(gsub("\n", "", remove_html_tags(x[, field_label])), "(", choices[, 2], ")")
        if (length(label) == 0)
          label = NA_character_
        levels = rep("c(0, 1)", length(choices[, 2]))
        labels_levels = rep("c(\"No\", \"Yes\")", length(choices[, 2]))
      } else {
        variable = x[, field_name]
        label = gsub("\n", "", remove_html_tags(x[, field_label]))
        if (length(label) == 0)
          label = NA_character_
        choices[, 2] = sapply(choices[, 2L], function(x) paste0("\"", x, "\""))
        levels = paste0("c(", paste0(unique(choices[, 1L]), collapse = ", "), ")")
        labels_levels = paste0("c(", paste0(unique(choices[, 2L]), collapse = ", "), ")")
      }
    } else if (tolower(x[, field_type]) == "yesno") {
      variable = x[, field_name]
      label = gsub("\n", "", remove_html_tags(x[, field_label]))
      if (length(label) == 0)
        label = NA_character_
      levels = "c(0, 1)"
      labels_levels = "c(\"No\", \"Yes\")"
    } else {
      variable =x[, field_name]
      label = gsub("\n", "", remove_html_tags(x[, field_label]))
      if (length(label) == 0)
        label = NA_character_
      levels = NA_character_
      labels_levels = NA_character_
    }
    value = data.table(Variable = variable, Label = label, Levels = levels, Label_Levels = labels_levels)
    value
  }

  labels_hash_table = metadata[, reshape_labels(.SD), by = key]
  labels_f_hash_table = labels_hash_table[!is.na(Levels), ]

  cmd = "\n\n# Convert categorical data to factors:\n\n"
  tmp = paste0(dataset_name, "$", labels_f_hash_table[, Variable], " = factor(",
               dataset_name, "$", labels_f_hash_table[, Variable], ", levels = ",
               labels_f_hash_table[, Levels], ", labels = ",
               labels_f_hash_table[, Label_Levels], ")")
  cmd = c(cmd, tmp)

  cmd = c(cmd, '\n\n#Assigning Hmisc labels to all variables:\n\nlibrary(Hmisc)\n')
  tmp = paste0("Hmisc::label(", dataset_name, "$", labels_hash_table[, Variable], ") = \"",
          labels_hash_table[, Label], "\"")
  cmd = c(cmd, tmp)
  cmd = paste0(cmd, collapse = "\n")
  cmd
}

#' @name get_status
#'
#' @title Get Cache status
#'
#' @description From the cache in the redcap object, identify the major events that have happened.
#'
#' @details This function helps format the cache entries so as to provide a meaningful decmdion of the events that happened during the object' lifecycle.
#'
#' This also helps in formatting output in the show command
#'
#' @param cache_objects Redcap object's cache
#' @param pretty Whether to format output for display
#'
#' @return Code that can be evaluated to format data

get_status = function(cache_objects, pretty = FALSE) {
  if (length(cache_objects) == 0) {
    if (pretty)
      return("{\tNo events yet!\t}")
    else
      return("No events yet")
  }
  message = character()
  if ("raw_records" %in% cache_objects)
    message = c(message, "records loaded (hint:use get_raw_data() to get raw data)")
  if ("raw_meta" %in% cache_objects)
    message = c(message, "metadata loaded (hint:use get_metadata() to get metadata)")
  if ("clean_records" %in% cache_objects)
    message = c(message, "records cleaned (hint:use get_clean_data() to get data with missing and out of range values set to NA)")
  if ("clean_meta" %in% cache_objects)
    message = c(message, "metadata munged (for internal use: <metaprogramming>)")
  if ("fmt_records" %in% cache_objects)
    message = c(message, "records formatted (hint:use get_formatted_data() to get clean data with variable and data labels plugged in)")
  if (pretty) {
    message = paste0("\t* ", message)
    message = paste0(message, collapse = "\n")
    message = paste0(" \n", message, "\n")
  }
  message
}

#' @name prepare_metadata_for_code_generation
#'
#' @title Prepare metadata for code generation
#'
#' @description Take metadata and make sure it conforms to the project's meta programming.
#'
#' @details This is a utility function that aids in preparing the metadata for cmd generation.
#' It converts the iput to a data table and assigns a key to it.
#'
#' @param metadata Redcap metadata
#'
#' @return transformed metadata
#'

prepare_metadata_for_code_generation = function(metadata) {
  if (!is_valid_metadata(metadata))
    stop("invalid metadata")
  metadata = data.table(metadata)
  metadata = metadata[, key := .I]
  setkey(metadata, key)
  metadata
}
