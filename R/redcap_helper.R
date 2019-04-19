#' @rdname GetChunks
#'
#' @name get_chunks
#'
#' @include generic_helper.R
#'
#' @title Get chunks of a specific size
#'
#' @description From a vector obtain a list of chunks of a specific size
#'
#' @return A list of objects of the same data type as input, each of size equal to the chunk size.
#'
#' The last chunk may be smaller if the input's length is not a multiple of the LCM of the input's length and the chunks size.
#'
#' @details This is a utility functiion that facilitates chunked operations.
#'
#' A common scenario is to chunk the indices of a specific object and then perform operations on the chunks separately and then later merge the output.
#'
#' This can be useful in situations where multiple operations of smaller scope is preferrable to bulk processing.
#'
#' @param x Input vector
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

#' @rdname GetChunkedRedcapData
#'
#' @name get_chunked_redcap_data
#'
#' @title Download REDCap data in chunks
#'
#' @description Download data in chunks from a REDCap repository using the REDCap api.
#'
#' Data and metadata are stored in the calling environment.
#'
#' @details The record identifiers are chunked and then data is is pulled for those specific records.
#'
#' The result is then combined into one data table.
#'
#' This might be handy in the case of network latency or when the data is large.
#'
#' This is just a convenient wrapper round the \code{\link{get_redcap_data}} function.
#'
#' In cases of strong bandwidth or small data sizes, just use \code{\link{get_redcap_data}}.
#'
#' @param api the REDCap instance's api location. Just append /api/ to the instance's url.
#' @param token The secret token for the project. Check the API page in REDCap. Must have api rights to access this.
#' @param local Whether the REDCap instance is local.
#' @param chunksize The size of the chunks to be pulled at a time.
#' @param forms A character vector of the list of forms to pull data from.
#' @param fields A character vector of the specific fields to pull data from.
#' @param ids_to_pull A character vector of the specific record itentifiers to pull from.
#' @param dataset_name Name of the resultant dataset
#' @param metadataset_name Name of the resultant metadata
#'
#' @seealso \code{\link{get_chunks}}, \code{\link{Redcap}}
#'
#' @export
#'
#' @family Data Input

get_chunked_redcap_data = function(api,
                                   token,
                                   local = TRUE,
                                   chunksize = 100,
                                   forms = NULL,
                                   fields = NULL,
                                   ids_to_pull = NULL,
                                   dataset_name = "records",
                                   metadataset_name = "meta") {
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
    assign(
      metadataset_name, get_redcap_data(api, token, local, content = "metadata"), outer_env
    )
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
    data_list = Map(function(ids) {
      ds_chunk = get_redcap_data(
        api = api, token = token, local = local, fields = fields, forms = forms, ids_to_pull = ids
      )
      message(paste0(
        "downloaded ", min(100, round(counter * 100 / data_size, 2)), "%", ifelse(counter >= data_size, "", "...")
      ))
      assign("counter", counter + chunksize, envir = parent.env(environment()))
      ds_chunk
    }, ids_list)
    assign(dataset_name, data.frame(data.table::rbindlist(data_list)), envir = outer_env)
  },
  error = function(e) {
    stop("chunked download failed: [details : ", sQuote(e$message), "]")
  },
  warning = function(w) {
    warning("chunked download failed: [details: ", sQuote(w$message), "]")
  })
}

#' @rdname GetBulkRedcapData
#'
#' @name get_redcap_data
#'
#' @title Bulk download of REDCap data
#'
#' @description Download data from a REDCap repository using the REDCap api in bulk.
#'
#' @return A data frame with the REDCap repo's data.
#'
#' @details A simulation of the posting of a form to the api is done and a connection is obtained.
#'
#' This connection is then used to read the data from REDCap into R.
#'
#' This is done in bulk. In the case  of network latency or big data \code{\link{get_chunked_redcap_data}} would be a better alternative for more responsiveness.
#'
#' @param api The REDCap instance's api location. Just append /api/ to the instance's url.
#' @param token The secret token for the project. Check the API page in REDCap. Must have api rights to access this.
#' @param content What to pull. Currently only `record` and `metadata` are supported.
#' @param local Whether the REDCap instance is local.
#' @param forms A character vector of the list of forms to pull data from.
#' @param fields A character vector of the specific fields to pull data from.
#' @param ids_to_pull A character vector of the specific record itentifiers to pull data from.
#' @seealso \code{\link{Redcap}}
#'
#' @export
#'
#' @family Data Input

get_redcap_data = function(api,
                           token,
                           content = "record",
                           local = TRUE,
                           forms = NULL,
                           fields = NULL,
                           ids_to_pull = NULL) {
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
    warning(w$message)
  })
  if (exists("redcap__err", envir = fun_env)) {
    msg = sQuote(get("redcap__err", envir = fun_env))
    rm("redcap__err", envir = fun_env)
    stop(paste0("data could not be downloaded [details: ", msg, "]"))
  } else {
    value = try(data.frame(read.csv(textConnection(redcap_conn), stringsAsFactors = FALSE)), silent = T)
    }
  if(class(value)!='try-error'){
    value
  }
}

#' @rdname IsMetadataValid
#'
#' @name is_valid_metadata
#'
#' @title Check whether REDCap metadata is valid
#'
#' @description This performs a check to validate the REDCap metadata provided.
#'
#' @details This makes sure that the REDCap metadata conforms to the expectations of this project.
#'
#' This helps avoid breaking of code in instances where metadata is required as an input.
#'
#' @param metadata REDCap metadata
#'
#' @return TRUE if valid else FALSE
#'

is_valid_metadata = function(metadata) {
  if (!is.data.frame(metadata))
    return(FALSE)
  value = FALSE
  if (all(
    c(
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
    ) %in% names(metadata)
  ))
  value = TRUE
  value
}

#' @rdname GetVariableNamesInRedcapDataset
#'
#' @name get_vars_in_data
#'
#' @title Get the names of variables in a dataset based on REDCap metadata
#'
#' @description Utility function that gets the name(s) of the variables in a redcap project.
#'
#' @details Using the redcap metadata, code is generated that extracts the names of the variables in the dataset.
#'
#' Checkboxes are also munged to reflect what is in the repository.
#'
#' Useful for metaprogramming.
#'
#' @param metadata REDCap metadata
#' @param negative_char checkbox expansion character for a negative checkbox level
#'
#' @return a character vector of the variable names in the dataset

get_vars_in_data = function(metadata, negative_char="_") {
  metadata = prepare_metadata_for_code_generation(metadata)
  get_vars_r = function(r) {
    var = r$field_name
    widget = r$field_type
    if (widget == "checkbox") {
      choices = r$select_choices_or_calculations
      choices = stringr::str_trim(unlist(strsplit(choices, "\\|")))
      choices = sapply(choices, function(ch) {
        lev = stringr::str_trim(unlist(strsplit(ch, ",")))[1L]
        lev = gsub("\\-|\\.", sprintf("\\%s", negative_char), lev)
        lev
      })
      value = data.table::data.table(var = paste0(var, "___", choices))
    } else if (widget == "descripive") {
      value = data.table::data.table()
    } else {
      value = data.table::data.table(var = var)
    }
    value
  }
  
  value = na.omit(metadata[, get_vars_r(.SD), by = key][, var])
  value
}

#' @rdname GetRDataTypesOfVariablesInRedcapDataset
#'
#' @name get_r_types_in_data
#'
#' @title Get the R data types of variables in dataset from REDCap metadata
#'
#' @description Utility function that gets the data type(s) of the variables in a redcap project
#'
#' @details Using the redcap metadata, code is generated that extracts the data types of the variables in the dataset.
#'
#' Useful for metaprogramming.
#'
#' @param metadata REDCap metadata
#' @param negative_char checkbox expansion character for a negative checkbox level
#'
#' @return The R data types of the variables in the dataset
#'
#'

get_r_types_in_data = function(metadata, negative_char="_") {
  metadata = data.table::data.table(metadata)
  metadata = metadata[, key:= .I]
  if (!is_valid_metadata(metadata))
    stop("metadata not valid")
  
  get_r_type_r = function(r) {
    widget = r$field_type
    if (widget == "checkbox") {
      choices = r$select_choices_or_calculations
      choices = sapply(strsplit(choices, "\\|"), function(ch) {
        lev = stringr::str_trim(unlist(strsplit(ch, ",")))[1L]
        lev = gsub("\\-|\\.", sprintf("\\%s", negative_char), lev)
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

#' @rdname GenerateCodeToRemoveCodedMissingValues
#'
#' @name generate_remove_missing_code
#'
#' @title Autogenerate code for removing coded missing values (set to NA) in REDCap data
#'
#' @description This is a utility function that employs code generation to produce r code for cleaning data.
#'
#' @details Using the redcap metadata, code is generated that removes coded missing data from repo.
#'
#' This is useful as missing data is coded in a variety of ways and this has to be reset to  missing for accurate data analysis especially in frequency counts, contingency tabling and modelling.
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded in place
#' @param negative_char checkbox expansion character for a negative checkbox level
#'
#' @export
#'
#' @return Code that can be evaluated to clean data
#'
#' @family Code Generators

generate_remove_missing_code = function(metadata, dataset_name = "data", negative_char="_") {
  metadata = prepare_metadata_for_code_generation(metadata)
  invalid_vals = c(
    "as.character(seq(as.Date(\"1910-01-01\"), as.Date(\"1950-01-01\"), by = \"year\"))",
    "\"-1\"", "\"Empty\"", "\"empty\"", "\"\""
  )
  invalid_vals = paste0(invalid_vals, collapse = ", ")
  invalid_vals = paste0("c(", invalid_vals, ")")
  
  cmd = stringr::str_trim(get_vars_in_data(metadata, negative_char))
  if (length(cmd) == 0L)
    return("")
  cmd = paste0(
    dataset_name, "$", cmd, "[stringr::str_trim(", dataset_name, "$", cmd, ") %in% ", invalid_vals, "] = NA"
  )
  cmd = c(
    "\n\n# Recoding coded missing entries to NA\n",
    "library(stringr)",
    paste0(dataset_name, " = data.frame(", dataset_name, ")"),
    cmd
  )
  cmd = paste0(cmd, collapse = "\n")
  cmd
}

#' @rdname GenerateCodeToRemoveOutliers
#'
#' @name generate_remove_outliers_code
#'
#' @title Autogenerate code for removing out of range values (set to NA) in REDCap data
#'
#' @description This is a utility function that employs code generation to produce r code for cleaning data.
#'
#' @details Using the redcap metadata, code is generated that removes out-of-range data from repo.
#'
#' This is necessary to avoid over or undeestimation during data analysis which often distorts the results.
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded
#'
#' @export
#'
#' @return Code that can be evaluated to clean data
#'
#' @family Code Generators

generate_remove_outliers_code = function(metadata, dataset_name = "data") {
  metadata = prepare_metadata_for_code_generation(metadata)
  check_miss = function(s) {
    is.na(s) | stringr::str_trim(s) == ""
  }
  has_min = !sapply(metadata$text_validation_min, check_miss)
  has_max = !sapply(metadata$text_validation_max, check_miss)
  has_valid = which(apply(cbind(has_min, has_max), 1, any, na.rm = TRUE))
  if (length(has_valid) == 0L)
    return("")
  metadata = metadata[has_valid]
  
  generate_code_r = function(r) {
    cmd = character(0L)
    var_r = stringr::str_trim(r$field_name)
    type_r = stringr::str_trim(r$text_validation_type_or_show_slider_number)
    min_r = stringr::str_trim(r$text_validation_min)
    max_r = stringr::str_trim(r$text_validation_max)
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
    has_min_r = all(!is.na(min_r), stringr::str_trim(min_r) != "")
    has_max_r = all(!is.na(max_r), stringr::str_trim(max_r) != "")
    
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
  cmd = paste0(
    "\n\n# Recoding out of range entries to NA\n\n",
    paste0(dataset_name, " = data.frame(", dataset_name, ")\n"),
    cmd
  )
  cmd
}


#' @rdname GenerateCodeForDateCasting
#'
#' @name generate_date_conversion_code
#'
#' @title Autogenerate code for date conversion from valid string date representations
#'
#' @description This is a utility function that employs code generation to produce R code for data recoding purposes.
#'
#' @details Using the redcap metadata, code is generated that converts character dates to R Date variables.
#'
#' This can come in handy when using the data for date-based operations such as subsetting or panel data analysis.
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded in place.
#' @param negative_char checkbox expansion character for a negative checkbox level
#'
#' @export
#'
#' @return Code that can be evaluated to recode apppropriate strings to R dates
#'
#' @family Code Generators

generate_date_conversion_code = function(metadata, dataset_name = "data", negative_char="_") {
  metadata = prepare_metadata_for_code_generation(metadata)
  metadata = metadata[stringr::str_trim(text_validation_type_or_show_slider_number) == "date_ymd"]
  if (nrow(metadata) < 1L)
    return("")
  cmd = get_vars_in_data(metadata, negative_char)
  cmd = paste0(dataset_name, "$", cmd, " = as.Date(", dataset_name, "$", cmd, ")")
  cmd = paste0(cmd, collapse = "\n")
  cmd = c(
    "\n\n# Converting valid string representation of dates to R `Date` objects\n",
    paste0(dataset_name, " = data.frame(", dataset_name, ")"),
    cmd
  )
  cmd = paste(cmd, collapse = "\n")
  cmd
}

#' @rdname GenerateCodeForFormatting
#'
#' @name generate_formatting_code
#'
#' @title Autogenerate code for data formatting (variable and data labelling)
#'
#' @description This is a utility function that employs code generation to produce R code that formats data.
#'
#' @details Using the redcap metadata, code is generated that formats data.
#'
#' It recodes categorical data to factors.
#'
#' This makes it easier to perform traditional statistical analysis which often expects coded categorical variables as input.
#'
#' @param metadata REDCap metadata
#' @param dataset_name Name of the dataset that will be recorded in place.
#' @param negative_char checkbox expansion character for a negative checkbox level
#'
#' @export
#'
#' @return Code that can be evaluated to format data.
#'
#' @family Code Generators
#'
#' @include branching_logic.R

generate_formatting_code = function(metadata, dataset_name = "data", negative_char="_") {
  metadata = prepare_metadata_for_code_generation(metadata)
  to_remove = paste0(unique(metadata[, form_name]),"_complete")
  metadata = metadata[!field_name %in% to_remove]
  reshape_labels = function(x) {
    if (tolower(x[, field_type]) %in% c("checkbox", "dropdown", "radio")) {
      choices =  t(sapply(unlist(strsplit(x[, select_choices_or_calculations], "\\|")),
                          function(ch) {
                            ch_ls = stringr::str_trim(unlist(regmatches(ch, regexpr(",", ch), invert = TRUE)))
                            names(ch_ls) = c("level", "label")
                            ch_ls
                          }))
      if (x[, field_type] == "checkbox") { 
        tmp = sapply(choices[, 1], function(x)
          gsub("\\-|\\.", negative_char, x))
        variable = paste0(x[, field_name], "___", tmp)
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
        choices[, 2] = sapply(choices[, 2L], function(x)
          paste0("\"", x, "\""))
        levels = paste0("c(", paste0(unique(choices[, 1L]), collapse = ", "), ")")
        labels_levels = paste0("c(", paste0(unique(choices[, 2L]), collapse = ", "), ")")
        if(length(unique(choices[, 1L]))!=length(unique(choices[, 2L]))){
        labels_levels = paste0("c(", paste0((choices[, 2L]), collapse = ", "), ")")
          }
        }
    } else if (tolower(x[, field_type]) == "yesno") {
      variable = x[, field_name]
      label = gsub("\n", "", remove_html_tags(x[, field_label]))
      if (length(label) == 0)
        label = NA_character_
      levels = "c(0, 1)"
      labels_levels = "c(\"No\", \"Yes\")"
    } else {
      variable = x[, field_name]
      label = gsub("\n", "", remove_html_tags(x[, field_label]))
      if (length(label) == 0)
        label = NA_character_
      levels = NA_character_
      labels_levels = NA_character_
    }
    value = data.table::data.table(
      Variable = variable, Label = label, Levels = levels, Label_Levels = labels_levels
    )
    value
  }
  
  labels_hash_table = metadata[, reshape_labels(.SD), by = key]
  labels_f_hash_table = labels_hash_table[!is.na(Levels),]
  
  cmd = "\n\n# Convert categorical data to factors:\n\n"
  tmp = paste0(
    dataset_name, "$", labels_f_hash_table[, Variable], " = factor(",
    dataset_name, "$", labels_f_hash_table[, Variable], ", levels = ",
    labels_f_hash_table[, Levels], ", labels = ",
    labels_f_hash_table[, Label_Levels], ")"
  )
  cmd = c(cmd, tmp)
  cmd = paste0(cmd, collapse = "\n")
  cmd
}

#' @rdname GenerateCodeForDataEntryValidation
#'
#' @name generate_data_validation_code
#'
#' @title Autogenerate code for data validation
#'
#' @description This is a utility function that employs code generation to produce R code that validates data entry workflow for errors of omission and commision.
#'
#' @details Using the redcap metadata, code is generated that validates data entry during the data capture process.
#'
#' This code is then evaluated into a function that is then iterated through the records to check for errors during data capture.
#'
#' The result is a dataset containing the resultant errors.
#'
#' @param metadata REDCap metadata with a formatted branching logic variable \emph{f.branching_logic}. See \code{\link{ExpandBranchingLogic}} 
#' @param date_var Name of variable that captures the date of entry
#' @param hosp_var Name of variable that holds the hospital code
#' @param surrogate_id_var Name of variable that holds a surrogate identifier that is easier to reference
#' @param custom_code Any code that is appended for custom plugin of special validation checks.
#' @param updates Name of a list of RedcapUpdate(s) to be used for plugging functionality that abstracts the introduction of new variables during the projects lifecycle. See \code{\link{RedcapUpdate}}
#' @param updates_envir_depth Integer of what parent frame contains updates. Default is immediate parent of calling environment (1) ie one level deep.
#' @param negative_char checkbox expansion character for a negative checkbox level
#'
#' @export
#'
#' @return A function that can be evaluated to validate data entry workflow for a single record.
#'
#' @family Code Generators
#'
#' @include branching_logic.R
#' @include data_types.R
#' @include script.R
#' @include expand_branching_logic.R

generate_data_validation_code = function(
  metadata, 
  date_var, 
  hosp_var, 
  surrogate_id_var, 
  custom_code = NA, 
  updates = NULL, 
  updates_envir_depth = 1, 
  negative_char="_") {
  metadata = prepare_metadata_for_code_generation(metadata)
  reset_tab()
  id_var = unlist(metadata[1, .SD, .SDcols = 1])[1]
  cmd = character()
  tmp = ""
  tmp = c(
    tmp, paste0(
      get_tab(), "validate_data_entry = function(data_row, hosp_to_validate = NA, updates = list()) {"
    )
  )
  add_tab()
  tmp = c(tmp, paste0(get_tab(), "if (!is.data.frame(data_row))"))
  add_tab()
  tmp = c(tmp, paste0(get_tab(), "stop(\"input is not a data frame\")"))
  remove_tab()
  tmp = c(tmp, paste0(get_tab(), "if (!(nrow(data_row) == 1))"))
  add_tab()
  tmp = c(tmp, paste0(get_tab(), "stop(\"input must have only one row\")"))
  remove_tab()
  tmp = c(tmp, paste0(get_tab(), "while (\"data_row\" %in% search())"))
  add_tab()
  tmp = c(tmp, paste0(get_tab(), "detach(data_row)"))
  remove_tab()
  tmp = c(tmp, paste0(get_tab(), "attach(data_row)"))
  tmp = c(tmp, paste0(get_tab(), "form__x2014cin = character()"))
  tmp = c(tmp, paste0(get_tab(), "sect__x2014cin = character()"))
  tmp = c(tmp, paste0(get_tab(), "name__x2014cin = character()"))
  tmp = c(tmp, paste0(get_tab(), "entry__x2014cin = character()"))
  tmp = c(tmp, paste0(get_tab(), "type__x2014cin = character()"))
  tmp = c(tmp, paste0(get_tab(), "msg__x2014cin = character()"))
  tmp = c(tmp, paste0(get_tab(), "logic_x2014cin = character()"))
  tmp = c(
    tmp, paste0(
      get_tab(), "if (!date_can_be_validated(", date_var, ")) return(data.table("
    )
  )
  add_tab()
  tmp = c(tmp, paste0(get_tab(), "RecordID = ", id_var))
  if (!is.na(surrogate_id_var))
    tmp = c(tmp, paste0(get_tab(), ",Identifier = ", surrogate_id_var))
  tmp = c(tmp, paste0(get_tab(), ",DateOfEntry = as.Date(NA)"))
  tmp = c(tmp, paste0(get_tab(), ",Hospital = ", hosp_var))
  tmp = c(tmp, paste0(get_tab(), ",Form = \"<< Before Data Evaluations >>\""))
  tmp = c(tmp, paste0(get_tab(), ",Name = \"<< Before Data Evaluations >>\""))
  tmp = c(
    tmp, paste0(
      get_tab(), ",Message = \"<< Date variable [", date_var, "] missing. This is needed for error reporting >>\""
    )
  )
  remove_tab()
  tmp = c(tmp, paste0(get_tab(), ")) else ", date_var, " = as.Date(", date_var,  ")"))
  if (!is.null(updates)) {
    tmp = c(
      tmp, paste0(
        get_tab(), "if (!all(sapply(updates, function(u) \"RedcapUpdate\" %in% class(u)))) stop(\"invalid updates\")"
      )
    )
    tmp = c(
      tmp, paste0(
        get_tab(), "if (!all(sapply(updates, function(x) x$is_valid()))) stop(\"invalid updates\")"
      )
    )
    tmp = c(tmp, paste0(get_tab(), ".__update = updates"))
  }
  tmp = paste0(tmp, collapse = "\n")
  cmd = c(cmd, tmp)
  rolling_fn_x2014cin = ''
  rolling_sc_x2014cin = ''
  gen_code_r = function(meta_r) {
    cmd_r = character()
    if (!exists("rolling_fn_x2014cin", envir = parent.frame()))
      assign("rolling_fn_x2014cin", "", envir = parent.frame())
    if (!exists("rolling_fn_x2014cin", envir = parent.frame()))
      assign("rolling_sc_x2014cin", "", envir = parent.frame())
    vname_x2014cin = stringr::str_trim(meta_r[, field_name])
    fname_x2014cin = toproper(gsub("_", " ", stringr::str_trim(meta_r[, form_name])), all = T)
    sectn_h_x2014cin = stringr::str_trim(meta_r[, section_header])
    sectn_h_x2014cin = if (isTRUE(any(
      is.null(sectn_h_x2014cin),
      is.na(sectn_h_x2014cin),
      stringr::str_trim(sectn_h_x2014cin) == ""
    ))) {
      if (isTRUE(fname_x2014cin == get("rolling_fn_x2014cin", envir = parent.frame()))) {
        get("rolling_sc_x2014cin", envir = parent.frame())
      } else {
        ""
      }
    } else {
      stringr::str_trim(remove_html_tags(sectn_h_x2014cin))
    }
    sectn_h_x2014cin = toproper(gsub("[\n\t]", "", sectn_h_x2014cin))
    if (isTRUE(fname_x2014cin != get("rolling_fn_x2014cin", envir = parent.frame()))) {
      assign("rolling_fn_x2014cin", fname_x2014cin, envir = parent.frame())
      assign("rolling_sc_x2014cin", sectn_h_x2014cin, envir = parent.frame())
    }
    vtype_x2014cin = stringr::str_trim(meta_r[, field_type])
    vlabel_x2014cin = stringr::str_trim(remove_html_tags(meta_r[, field_label]))
    vlabel_x2014cin = gsub("[\n\t]", "", vlabel_x2014cin)
    vtype_val_x2014cin = stringr::str_trim(meta_r[, text_validation_type_or_show_slider_number])
    vtype_val_x2014cin = if (isTRUE(any(
      is.na(vtype_val_x2014cin), stringr::str_trim(vtype_val_x2014cin) == ""
    ))) {
      NA
    } else {
      stringr::str_trim(vtype_val_x2014cin)
    }
    choices_x2014cin = stringr::str_trim(meta_r[, select_choices_or_calculations])
    choices_x2014cin = if (isTRUE(any(
      is.na(choices_x2014cin), stringr::str_trim(choices_x2014cin) == ""
    ))) {
      NA
    } else {
      stringr::str_trim(choices_x2014cin)
    }
    min_val_x2014cin = stringr::str_trim(meta_r[, text_validation_min])
    min_val_x2014cin = if (isTRUE(any(
      is.na(min_val_x2014cin), stringr::str_trim(min_val_x2014cin) == ""
    ))) {
      NA
    } else {
      stringr::str_trim(min_val_x2014cin)
    }
    max_val_x2014cin = stringr::str_trim(meta_r[, text_validation_max])
    max_val_x2014cin = if (isTRUE(any(
      is.na(max_val_x2014cin), stringr::str_trim(max_val_x2014cin) == ""
    ))) {
      NA
    } else {
      stringr::str_trim(max_val_x2014cin)
    }
    logic_x2014cin = stringr::str_trim(meta_r[, f.branching_logic])
    logic_x2014cin = if (isTRUE(any(
      is.na(logic_x2014cin), stringr::str_trim(logic_x2014cin) == ""
    ))) {
      NA
    } else {
      stringr::str_trim(logic_x2014cin)
    }
    req_x2014cin = stringr::str_trim(meta_r[, required_field])
    req_x2014cin = if (isTRUE(any(
      is.na(req_x2014cin), stringr::str_trim(req_x2014cin) == ""
    ))) {
      NA
    } else {
      stringr::str_trim(req_x2014cin)
    }
    get_checkbx_logic = function() {
      chk_tmp = stringr::str_trim(unlist(strsplit(choices_x2014cin, "\\|")))
      chk_tmp = sapply(chk_tmp, function(chk) {
        value = as.numeric(stringr::str_trim(unlist(strsplit(chk, ",")))[1])
        if (value < 0)
          value = gsub("\\-|\\.", sprintf("\\%s", negative_char), as.character(value))
        value = as.character(value)
      })
      chk_cmd = paste0(vname_x2014cin, "___", chk_tmp)
      chk_cmd = paste0("data_missing(", chk_cmd, ") ")
      add_tab()
      chk_cmd = paste0(chk_cmd, "\n")
      chk_cmd = paste0(chk_cmd, collapse = paste0(get_tab(), ","))
      chk_cmd = paste0(paste0("\n", get_tab(), " "), chk_cmd)
      remove_tab()
      chk_cmd
    }
    to_validate = isTRUE(any(
      isTRUE(tolower(req_x2014cin) == "y"),
      isTRUE(vtype_val_x2014cin %in% c("integer", "number", "date_ymd")),
      isTRUE(any(
        !is.na(min_val_x2014cin),!is.na(max_val_x2014cin)
      ))
    ))
    if (!to_validate)
      return(data.table::data.table())
    if (!is.null(updates)) {
      if (isTRUE(stringr::str_trim(vtype_x2014cin) == "checkbox")) {
        tmp = stringr::str_trim(unlist(strsplit(choices_x2014cin, "\\|")))
        tmp = sapply(tmp, function(ch) {
          lev = stringr::str_trim(unlist(strsplit(ch, ",")))[1]
          lev = gsub("\\-|\\.", sprintf("\\%s", negative_char), lev)
          lev
        })
        tmp = paste0(vname_x2014cin, "___", tmp)
        cmd_r = c(cmd_r, paste0(
          get_tab(), ".__update_date = c(", paste0("\n\t", get_tab()), paste0(
            paste0(
              "lapply(.__update, function (x) x$get_update_date(\"", tmp, "\", ", hosp_var, "))"
            )
            , collapse = paste0(",\n\t", get_tab())
          ), ")"
        ))
      } else {
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), ".__update_date = lapply(.__update, function (x) x$get_update_date(\"", vname_x2014cin, "\", ", hosp_var, "))"
          )
        )
      }
      cmd_r = c(cmd_r, paste0(get_tab(), ".__update_date = Reduce(c, .__update_date)"))
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), ".__update_date = if (is.null (.__update_date)) NA else .__update_date"
        )
      )
      cmd_r = c(cmd_r, paste0(get_tab(), "if (length(na.omit(.__update_date)) > 0)"))
      add_tab()
      cmd_r = c(cmd_r, paste0(
        get_tab(), ".__update_date = max(.__update_date, na.rm = T)"
      ))
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), "else"))
      add_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), ".__update_date = NA"))
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), ".__is_update = !is.na(.__update_date)"))
    }
    if (isTRUE(tolower(req_x2014cin) == "y")) {
      if (!is.na(logic_x2014cin)) {
        cmd_r = c(cmd_r, paste0(get_tab(), "if (isTRUE(", logic_x2014cin, ")) {"))
        add_tab()
      }
      if (isTRUE(stringr::str_trim(vtype_x2014cin) == "checkbox"))
        cmd_r = c(cmd_r, paste0(
          get_tab(), "if (isTRUE(all(", get_checkbx_logic(), "))) {"
        ))
      else
        cmd_r = c(cmd_r, paste0(
          get_tab(), "if (isTRUE(data_missing(", vname_x2014cin, "))) {"
        ))
      add_tab()
      if (!is.null(updates)) {
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "if (! isTRUE(.__is_update & ", date_var, " <= .__update_date)) {"
          )
        )
        add_tab()
      }
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "form__x2014cin = c(form__x2014cin, \"", toproper(fname_x2014cin, all = T), "\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "sect__x2014cin = c(sect__x2014cin, \"", toproper(sectn_h_x2014cin), "\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "name__x2014cin = c(name__x2014cin, \"", vname_x2014cin, "\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "entry__x2014cin = c(entry__x2014cin, as.character(", vname_x2014cin, "))"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "type__x2014cin = c(type__x2014cin, \"Required Entry\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "msg__x2014cin = c(msg__x2014cin, \"'", vlabel_x2014cin, "' is required!\")"
        )
      )
      if(is.na(logic_x2014cin)){
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "logic_x2014cin = c(logic_x2014cin,", logic_x2014cin, ")"
          )
        )
      }else{
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "logic_x2014cin = c(logic_x2014cin, \"", logic_x2014cin, "\" )"
          )
        )
      }
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      if (!is.null(updates)) {
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
      if (!is.na(logic_x2014cin)) {
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
    }
    if (isTRUE(vtype_val_x2014cin %in% c("integer", "number", "date_ymd"))) {
      if (!is.na(logic_x2014cin)) {
        cmd_r = c(cmd_r, paste0(get_tab(), "if (isTRUE(", logic_x2014cin, ")) {"))
        add_tab()
      }
      cmd_r = c(cmd_r, paste0(
        get_tab(), "if (! isTRUE(data_missing(", vname_x2014cin, "))) {"
      ))
      add_tab()
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "if ((data_can_be_validated(", vname_x2014cin, "))) {"
        )
      )
      add_tab()
      if (!is.null(updates)) {
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "if (! isTRUE(.__is_update & ", date_var, " <= .__update_date)) {"
          )
        )
        add_tab()
      }
      if (isTRUE(tolower(vtype_val_x2014cin) == "date_ymd")) {
        cmd_r = c(cmd_r, paste0(
          get_tab(), "if (! isTRUE(is_date(", vname_x2014cin, "))) {"
        ))
        add_tab()
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "form__x2014cin = c(form__x2014cin, \"", toproper(fname_x2014cin, all = T), "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "sect__x2014cin = c(sect__x2014cin, \"", toproper(sectn_h_x2014cin), "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "name__x2014cin = c(name__x2014cin, \"", vname_x2014cin, "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "entry__x2014cin = c(entry__x2014cin, as.character(", vname_x2014cin, "))"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "type__x2014cin = c(type__x2014cin, \"Invalid Data Type\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "msg__x2014cin = c(msg__x2014cin, \"'", vlabel_x2014cin, "' must be a date!\")"
          )
        )
        if(is.na(logic_x2014cin)){
          cmd_r = c(
            cmd_r, paste0(
              get_tab(), "logic_x2014cin = c(logic_x2014cin,", logic_x2014cin, ")"
            )
          )
        }else{
          cmd_r = c(
            cmd_r, paste0(
              get_tab(), "logic_x2014cin = c(logic_x2014cin, \"", logic_x2014cin, "\" )"
            )
          )
        }
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
      else if (isTRUE(tolower(vtype_val_x2014cin) == "number")) {
        cmd_r = c(cmd_r, paste0(
          get_tab(), "if (! isTRUE(is_number(", vname_x2014cin, "))) {"
        ))
        add_tab()
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "form__x2014cin = c(form__x2014cin, \"", toproper(fname_x2014cin, all = T), "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "sect__x2014cin = c(sect__x2014cin, \"", toproper(sectn_h_x2014cin), "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "name__x2014cin = c(name__x2014cin, \"", vname_x2014cin, "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "entry__x2014cin = c(entry__x2014cin, as.character(", vname_x2014cin, "))"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "type__x2014cin = c(type__x2014cin, \"Invalid Data Type\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "msg__x2014cin = c(msg__x2014cin, \"'", vlabel_x2014cin, "' must be a number!\")"
          )
        )
        if(is.na(logic_x2014cin)){
          cmd_r = c(
            cmd_r, paste0(
              get_tab(), "logic_x2014cin = c(logic_x2014cin,", logic_x2014cin, ")"
            )
          )
        }else{
          cmd_r = c(
            cmd_r, paste0(
              get_tab(), "logic_x2014cin = c(logic_x2014cin, \"", logic_x2014cin, "\" )"
            )
          )
        }
        
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
      else if (isTRUE(tolower(vtype_val_x2014cin) == "integer")) {
        cmd_r = c(cmd_r, paste0(
          get_tab(), "if (! isTRUE(is_int(", vname_x2014cin, "))) {"
        ))
        add_tab()
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "form__x2014cin = c(form__x2014cin, \"", toproper(fname_x2014cin, all = T), "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "sect__x2014cin = c(sect__x2014cin, \"", toproper(sectn_h_x2014cin), "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "name__x2014cin = c(name__x2014cin, \"", vname_x2014cin, "\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "entry__x2014cin = c(entry__x2014cin, as.character(", vname_x2014cin, "))"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "type__x2014cin = c(type__x2014cin, \"Invalid Data Type\")"
          )
        )
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "msg__x2014cin = c(msg__x2014cin, \"'", vlabel_x2014cin, "' must be an integer!\")"
          )
        )
        if(is.na(logic_x2014cin)){
          cmd_r = c(
            cmd_r, paste0(
              get_tab(), "logic_x2014cin = c(logic_x2014cin,", logic_x2014cin, ")"
            )
          )
        }else{
          cmd_r = c(
            cmd_r, paste0(
              get_tab(), "logic_x2014cin = c(logic_x2014cin, \"", logic_x2014cin, "\" )"
            )
          )
        }
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
      else
        stop("Only dates, numbers and integers to be validated here!")
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      if (!is.null(updates)) {
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
      if (!is.na(logic_x2014cin)) {
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
    }
    if (isTRUE(any(!is.na(min_val_x2014cin),!is.na(max_val_x2014cin)))) {
      has_min_x2014cin = !is.na(min_val_x2014cin)
      has_max_x2014cin = !is.na(max_val_x2014cin)
      if (tolower(vtype_val_x2014cin) == "date_ymd") {
        if (has_min_x2014cin)
          min_val_x2014cin = paste0("as.Date(\"", min_val_x2014cin, "\")")
        if (has_max_x2014cin)
          max_val_x2014cin = paste0("as.Date(\"", max_val_x2014cin, "\")")
        tmp_var = paste0("as.Date(", vname_x2014cin, ")")
      } else {
        tmp_var = paste0("suppressWarnings(as.numeric(", vname_x2014cin, "))")
      }
      if (!is.na(logic_x2014cin)) {
        cmd_r = c(cmd_r, paste0(get_tab(), "if (isTRUE(", logic_x2014cin, ")) {"))
        add_tab()
      }
      cmd_r = c(cmd_r, paste0(
        get_tab(), "if (! isTRUE(data_missing(", vname_x2014cin, "))) {"
      ))
      add_tab()
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "if (isTRUE(data_can_be_validated (", vname_x2014cin, "))) {"
        )
      )
      add_tab()
      if (!is.null(updates)) {
        cmd_r = c(
          cmd_r, paste0(
            get_tab(), "if (! isTRUE(.__is_update & ", date_var, " <= .__update_date)) {"
          )
        )
        add_tab()
      }
      range_code_x2014cin = "if (isTRUE("
      if (has_min_x2014cin)
        range_code_x2014cin = paste0(range_code_x2014cin, tmp_var, "!=-1 & (",tmp_var, " < ", min_val_x2014cin)
      if (has_min_x2014cin & has_max_x2014cin)
        range_code_x2014cin = paste0(range_code_x2014cin, " | ")
      if (has_max_x2014cin)
        range_code_x2014cin = paste0(range_code_x2014cin, tmp_var, " > ", max_val_x2014cin)
      range_code_x2014cin = paste0(range_code_x2014cin, "))) {")
      cmd_r = c(cmd_r, paste0(get_tab(), range_code_x2014cin))
      add_tab()
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "form__x2014cin = c(form__x2014cin, \"", toproper(fname_x2014cin, all = T), "\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "sect__x2014cin = c(sect__x2014cin, \"", toproper(sectn_h_x2014cin), "\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "name__x2014cin = c(name__x2014cin, \"", vname_x2014cin, "\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "entry__x2014cin = c(entry__x2014cin, as.character(", vname_x2014cin, "))"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "type__x2014cin = c(type__x2014cin, \"Out of Range\")"
        )
      )
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "msg__x2014cin = c(msg__x2014cin, \"'", vlabel_x2014cin, "' is out of range!\")"
        )
      )
    if(is.na(logic_x2014cin)){
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "logic_x2014cin = c(logic_x2014cin,", logic_x2014cin, ")"
        )
      )
    }else{
      cmd_r = c(
        cmd_r, paste0(
          get_tab(), "logic_x2014cin = c(logic_x2014cin, \"", logic_x2014cin, "\" )"
        )
      )
    }
      
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      if (!is.null(updates)) {
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      remove_tab()
      cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      if (!is.na(logic_x2014cin)) {
        remove_tab()
        cmd_r = c(cmd_r, paste0(get_tab(), "}"))
      }
    }
    if (!is.null(updates)) {
      cmd_r = c(cmd_r, paste0(get_tab(), "rm(.__update_date)"))
      cmd_r = c(cmd_r, paste0(get_tab(), "rm(.__is_update)"))
    }
    cmd_r
  }
  tmp = metadata[, gen_code_r(.SD), by = key]
  tmp = tmp[!is.na(V1), V1]
  add_tab()
  tmp = paste0(get_tab(), tmp)
  remove_tab()
  tmp = c(
    paste0(get_tab(), ""),
    paste0(get_tab(), "# <Autogenerated code starts here>"),
    paste0(get_tab(), "{"),
    tmp,
    paste0(get_tab(), "}"),
    paste0(get_tab(), "# <Autogenerated code ends here>")
  )
  tmp = paste0(tmp, collapse = "\n")
  cmd = c(cmd, tmp)
  custom_code = custom_code[!is.na(custom_code) |
                              stringr::str_trim(custom_code) == ""]
  if (length(na.omit(custom_code)) != 0) {
    custom_code = custom_code[!is.na(custom_code) |
                                stringr::str_trim(custom_code) == ""]
    if (length(custom_code) > 0) {
      custom_code = sapply(custom_code, convert_space2tab)
      add_tab()
      custom_code = paste0(get_tab(), custom_code)
      remove_tab()
      custom_code = c(
        paste0(get_tab(), ""),
        paste0(get_tab(), "# <Custom code starts here>"),
        paste0(get_tab(), "{"),
        custom_code,
        paste0(get_tab(), "}"),
        paste0(get_tab(), "# <Custom code ends here>\n")
      )
      custom_code = paste0(custom_code, collapse = "\n")
      cmd = c(cmd, custom_code)
    }
  }
  tmp = character()
  tmp = c(tmp, paste0(get_tab(), "flush.console()
           cat(paste('Record ID:', data_row$",id_var, ", 'validated\n'))
                      if (length(msg__x2014cin) > 0L) {"))
  add_tab()
  tmp = c(tmp, paste0(
    get_tab(), "id_x2014cin = rep(", id_var, ", length(msg__x2014cin))"
  ))
  if (!is.na(surrogate_id_var))
    tmp = c(tmp, paste0(
      get_tab(), "surr_id_x2014cin = rep(", surrogate_id_var, ", length(msg__x2014cin))"
    ))
  tmp = c(tmp, paste0(
    get_tab(), "date_x2014cin = rep(", date_var, ", length(msg__x2014cin))"
  ))
  tmp = c(tmp, paste0(
    get_tab(), "hosp_x2014cin = rep(", hosp_var, ", length(msg__x2014cin))"
  ))
  tmp = c(
    tmp, paste0(
      get_tab(), paste0(
        "value_x2014cin = data.table::data.table(RecordID = id_x2014cin,",
        ifelse(is.na(surrogate_id_var), "", " Identifier = surr_id_x2014cin,"),
        " DateOfEntry = date_x2014cin, Hospital = hosp_x2014cin, Form = form__x2014cin, Section = sect__x2014cin, Variable = name__x2014cin, Type = type__x2014cin, Entry = entry__x2014cin, Message = msg__x2014cin, Logic=logic_x2014cin
        )"
      )
    )
  )
  remove_tab()
  tmp = c(tmp, paste0(get_tab(), "}"))
  tmp = c(tmp, paste0(get_tab(), "else {"))
  add_tab()
  tmp = c(tmp, paste0(get_tab(), "value_x2014cin = data.table::data.table()"))
  remove_tab()
  tmp = c(tmp, paste0(get_tab(), "}"))
  tmp = c(tmp, paste0(get_tab(), "detach(data_row)"))
  tmp = c(tmp, paste0(get_tab(), "value_x2014cin"))
  remove_tab()
  tmp = c(tmp, paste0(get_tab(), "}"))
  tmp = paste0(tmp, collapse = "\n")
  reset_tab()
  cmd = c(
    "\n# <Note: !! Do not modify this function as it may change in future code regenerations !!>\n", cmd, tmp
  )
  cmd = paste0(cmd, collapse = "\n")
  cmd
}


#' @rdname GetCacheStatus
#'
#' @name get_status
#'
#' @title Get Cache status
#'
#' @description From the cache in the redcap object, identify the major events that have ocurred during the lifetime of a Redcap object
#'
#' @details This function helps format the cache entries so as to provide a meaningful description of the events that happened during the object' lifecycle.
#'
#' This also helps in formatting output in the show command.
#'
#' @param cache_objects Redcap object's cache
#' @param pretty Whether to format output for display
#'
#' @return Code that can be evaluated to format cache status

get_status = function(cache_objects, pretty = FALSE) {
  if (length(cache_objects) == 0) {
    if (!pretty)
      return("No events yet. (hint) use `obj`$load_data() to load data into memory.")
    else
      return("\nNo events yet. (hint) use `obj`$load_data() to load data into memory.\n")
  }
  message = character()
  if ("raw_records" %in% cache_objects)
    message = c(message, "records loaded. (hint) use `obj`$get_raw_data() to get raw data.")
  if ("raw_meta" %in% cache_objects)
    message = c(message, "metadata loaded. (hint) use `obj`$get_metadata() to get metadata.")
  if ("fmt_records" %in% cache_objects)
    message = c(
      message, "records formatted. (hint) use `obj`$get_formatted_data() to get data with data labels plugged in (factors)."
    )
  if ("clean_records" %in% cache_objects)
    message = c(
      message, "records cleaned. (hint) use `obj`$get_clean_data() to get formatted data with coded missing values set to NA."
    )
  if ("clean_meta" %in% cache_objects)
    message = c(message, "metadata munged. (for internal use - code generation)")
  if ("validate_data_entry" %in% cache_objects)
    message = c(
      message, "error report code in memory.  (hint) use `obj`$get_error_report() to get error report."
    )
  if ("err_rpt" %in% cache_objects)
    message = c(
      message, "error report created. (hint) use `obj`$get_error_report() to get error report."
    )
  if (pretty) {
    message = paste0(">> ", message)
    message = paste0(message, collapse = "\n")
    message = paste0("\n", message, "\n")
  }
  message
}

#' @rdname PrepareMetadataForCodeGeneration
#'
#' @name prepare_metadata_for_code_generation
#'
#' @title Prepare metadata for code generation
#'
#' @description Take metadata and make sure it conforms to the project's metaprogramming expected format.
#'
#' @details This is a utility function that aids in preparing the metadata for cmd generation.
#'
#' It converts the input to a data table and assigns a key to it and also removes unnecessary fields.
#'
#' @param metadata REDCap metadata
#'
#' @return transformed metadata
#'

prepare_metadata_for_code_generation = function(metadata) {
  if (!is_valid_metadata(metadata))
    stop("invalid metadata")
  metadata = data.frame(lapply(metadata, as.character), stringsAsFactors = FALSE)
  metadata = data.table::data.table(metadata)
  metadata = metadata[, key:= .I]
  setkey(metadata, key)
  metadata = metadata[tolower(field_type) != "descriptive"]
  metadata = metadata[tolower(field_type) == "checkbox", required_field:= "Y", by = key]
  metadata
}

#' @rdname GetRedcapVersion
#'
#' @name get_redcap_version
#'
#' @title Get REDCap version
#'
#' @description Get the version of REDCap associated with a specific url
#'
#' @return A list containing the version info
#' 
#' This includes the major, minor and release number version information for the REDCap instance 
#'
#' @details This function gets the REDCap version number by web scraping the specific instance's index page.
#' 
#' The footer is extracted which contains the version number.
#' 
#' This is then wranged into the final version information.
#'
#' @param url The url of the REDCap instance
#'
#' @seealso \code{\link{redcap_project}}
#'
#' @export
#'

get_redcap_version <- function(url = "http://localhost/redcap") {
  if (!url.exists(url) || !grepl("/redcap(/)?", url))
    stop(sprintf("invalid redcap url %s", sQuote(url)))
  pattern <- "REDCap([[:space:][:alpha:]\\-])+[[:digit:]]+.[[:digit:]]+.[[:digit:]]"
  page <- readLines(url, warn=F)
  version <- page[sapply(page, regexpr, pattern=pattern) > 0]
  if (0 == length(version))
    stop(sprintf("cannot get version info from index page %s", sQuote(url)))
  version <- regmatches(version[1], regexpr(pattern, version))
  version <- regmatches(
    version, 
    regexpr("[[:digit:]]+.[[:digit:]]+.[[:digit:]]", version)
    )
  setNames(
    lapply(unlist(strsplit(version, "\\.")), as.integer), 
    c("major", "minor", "release"))
}