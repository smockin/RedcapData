#' @include data_types.R
NULL

#' @include data_checks.R
NULL

#' @include branching_logic.R
NULL

# R CMD check's static analysis can't see that these are data.table column names
# referenced via NSE (e.g. meta[field_type == "checkbox"]), not undefined globals --
# this is the standard, documented workaround for that class of false positive.
# See "Importing data.table" in the data.table package vignettes.
utils::globalVariables(c(
  "field_type", "branching_logic", "f.branching_logic", "..subcols",
  "text_validation_min", "text_validation_max", "N_Missing"
))

# NOTE: data.table, stringr, and openxlsx are all brought in via the package-wide
# @import declarations in generic_helper.R (which generate real NAMESPACE import()
# entries) -- that's what actually makes their functions resolvable from inside these
# package functions. require()/library() calls here would only attach them to the
# search path at load time, which does *not* affect name lookup within a package's own
# functions and would not fix a "could not find function" error for them.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Extract bare variable-name tokens referenced in an R-translated branching logic
# condition, so we know which columns to coerce/bind when evaluating it.
.dq_extract_vars <- function(cond) {
  if (is.na(cond) || str_trim(cond) == "")
    return(character(0))
  toks <- regmatches(cond, gregexpr("[a-zA-Z_][a-zA-Z0-9_.]*", cond))[[1]]
  # drop known function/keyword tokens that can appear in translated logic
  reserved <- c("as", "Date", "as.Date", "TRUE", "FALSE", "T", "F", "NA", "isTRUE",
                "is", "na", "is.na", "any", "all", "str_trim")
  toks <- unique(toks)
  toks[!toks %in% reserved]
}

# Evaluate a (possibly NA) branching-logic condition, vectorized, across the whole dataset.
# Returns a logical vector of length nrow(data): TRUE = record is eligible for this field.
# Never errors: falls back to "eligible for everyone" + a note on failure.
.dq_eval_eligibility <- function(cond, data, notes_env) {
  n <- nrow(data)
  if (is.null(cond) || is.na(cond) || str_trim(cond) == "")
    return(rep(TRUE, n))

  vars_needed <- .dq_extract_vars(cond)
  vars_present <- intersect(vars_needed, names(data))
  vars_missing <- setdiff(vars_needed, names(data))

  if (length(vars_missing) > 0) {
    assign(cond, paste0(
      "referenced field(s) not found in data: ", paste(vars_missing, collapse = ", "),
      " -- treated as eligible for all records"
    ), envir = notes_env)
    return(rep(TRUE, n))
  }

  eval_env <- new.env(parent = baseenv())
  for (v in vars_present) {
    col <- data[[v]]
    num <- suppressWarnings(as.numeric(col))
    # only coerce to numeric if that doesn't silently blow away real (non-missing) values
    if (all(is.na(num) == (is.na(col) | str_trim(as.character(col)) == ""))) {
      assign(v, num, envir = eval_env)
    } else {
      assign(v, as.character(col), envir = eval_env)
    }
  }

  result <- tryCatch(
    eval(parse(text = cond), envir = eval_env),
    error = function(e) e
  )
  if (inherits(result, "error") || !is.logical(result) || length(result) != n) {
    assign(cond, paste0(
      "condition could not be evaluated (",
      if (inherits(result, "error")) conditionMessage(result) else "unexpected result shape",
      ") -- treated as eligible for all records"
    ), envir = notes_env)
    return(rep(TRUE, n))
  }
  result[is.na(result)] <- FALSE
  result
}

.dq_is_missing <- function(x, missing_codes) {
  xc <- str_trim(as.character(x))
  is.na(x) | xc == "" | xc %in% missing_codes
}

# as.Date() throws a hard ERROR (not NA) for non-empty strings it can't parse -- unlike
# as.numeric(), which just warns and returns NA. Worse, that error is thrown for the whole
# vectorized call: if even ONE element in a batch is unparseable, as.Date() aborts before
# producing results for any of them, so catching the error and falling back naively would
# wipe out every value in the batch, not just the bad one. This only ever calls as.Date()
# on the subset of strings that already look like a plain YYYY-MM-DD date; anything else
# (coded-missing values like "-1"/"-3", free text, malformed entries) is left as NA without
# ever being passed to as.Date() in the first place.
.dq_safe_as_date <- function(x) {
  x <- as.character(x)
  out <- as.Date(rep(NA_character_, length(x)))
  looks_like_date <- grepl("^[0-9]{4}[/-][0-9]{2}[/-][0-9]{2}$", str_trim(x))
  if (any(looks_like_date)) {
    parsed <- suppressWarnings(as.Date(str_trim(x[looks_like_date])))
    out[looks_like_date] <- parsed
  }
  out
}

# ---------------------------------------------------------------------------
# Completeness
# ---------------------------------------------------------------------------

#' @rdname compute_completeness
#'
#' @name compute_completeness
#'
#' @title Compute per-field completeness (missingness), adjusted for branching logic
#'
#' @description For every non-descriptive field in \code{metadata}, computes how many
#' records were actually eligible to have a value for that field (per its branching logic,
#' evaluated vectorized across the whole dataset), and how many of those eligible records
#' are missing it. This is the completeness computation used internally by
#' \code{\link{generate_data_quality_report}}; call it directly if you want the numbers
#' without writing an Excel file.
#'
#' @details Checkbox fields are judged as a group: a checkbox field is "missing" for a
#' record only if every one of its choice sub-columns is unset (0/NA/blank) for that record.
#' Any single choice selected counts the whole group as answered.
#'
#' If a field's branching logic condition cannot be evaluated (e.g. it references a column
#' not present in \code{data}), the field is conservatively treated as eligible for every
#' record, and a note explaining why is recorded in the returned \code{logic_notes} table.
#'
#' @param data Raw REDCap data (data.frame or data.table), one row per record, column names
#' matching \code{metadata$field_name} (checkbox fields already expanded to
#' \code{field___choice} columns, as REDCap exports them)
#' @param metadata REDCap metadata / data dictionary (data.frame or data.table)
#' @param missing_codes Character vector of raw values that should be treated as coded
#' missing data (in addition to \code{NA} and blank/whitespace-only strings). Defaults to
#' \code{c("-1", "-3", "Empty", "empty")}, REDCap's common "not recorded"/"not readable"
#' conventions
#' @param negative_char Checkbox expansion character for negative choice codes (matches the
#' rest of the package, e.g. \code{get_negative_char()} on a \code{Redcap} object)
#'
#' @return A list with three elements: \code{completeness} (a data.table, one row per
#' field, with \code{N_Eligible}, \code{N_Complete}, \code{N_Missing}, \code{Pct_Complete}),
#' \code{logic_notes} (branching logic conditions that could not be evaluated, if any), and
#' \code{n_total} (total record count in \code{data})
#'
#' @seealso \code{\link{generate_data_quality_report}}, \code{\link{compute_implausibility}}
#'
#' @export compute_completeness
#'
compute_completeness <- function(data, metadata, missing_codes = c("-1", "-3", "Empty", "empty"),
                                 negative_char = "_") {
  setDT(data)
  meta <- copy(as.data.table(metadata))
  meta <- meta[tolower(field_type) != "descriptive"]
  meta[, key := .I]
  meta[, f.branching_logic := NA_character_]
  meta[str_trim(branching_logic) != "", f.branching_logic := convert_redcap2r(branching_logic), by = key]

  notes_env <- new.env()
  n_total <- nrow(data)
  rows <- vector("list", nrow(meta))

  for (i in seq_len(nrow(meta))) {
    m <- meta[i]
    fname <- m$field_name
    ftype <- tolower(m$field_type)
    eligible <- .dq_eval_eligibility(m$f.branching_logic, data, notes_env)
    n_eligible <- sum(eligible)

    if (ftype == "checkbox") {
      subcols <- grep(paste0("^", fname, "___"), names(data), value = TRUE)
      if (length(subcols) == 0) {
        n_missing <- NA_integer_
        note <- "no expanded choice columns found in data (field___choice) -- skipped"
      } else {
        sub <- data[, ..subcols]
        answered <- Reduce(`|`, lapply(sub, function(col) {
          v <- suppressWarnings(as.numeric(col))
          !is.na(v) & v == 1
        }))
        n_missing <- sum(eligible & !answered)
        note <- ""
      }
    } else {
      if (!fname %in% names(data)) {
        n_missing <- NA_integer_
        note <- "field not found in data -- skipped"
      } else {
        miss <- .dq_is_missing(data[[fname]], missing_codes)
        n_missing <- sum(eligible & miss)
        note <- ""
      }
    }

    n_complete <- if (is.na(n_missing)) NA_integer_ else n_eligible - n_missing
    pct_complete <- if (is.na(n_missing) || n_eligible == 0) NA_real_ else round(100 * n_complete / n_eligible, 1)

    rows[[i]] <- data.table(
      Form = m$form_name,
      Section = if ("section_header" %in% names(m)) remove_html_tags(m$section_header) else "",
      Variable = fname,
      Label = remove_html_tags(m$field_label),
      FieldType = m$field_type,
      BranchingLogic = m$branching_logic,
      N_Eligible = n_eligible,
      N_Complete = n_complete,
      N_Missing = n_missing,
      Pct_Complete = pct_complete,
      Note = note
    )
  }

  out <- rbindlist(rows)
  notes <- if (length(ls(notes_env)) == 0) {
    data.table(BranchingLogic = character(0), Note = character(0))
  } else {
    data.table(BranchingLogic = ls(notes_env), Note = unlist(mget(ls(notes_env), envir = notes_env)))
  }
  list(completeness = out, logic_notes = notes, n_total = n_total)
}

# ---------------------------------------------------------------------------
# Implausibility
# ---------------------------------------------------------------------------

.dq_date_types <- c("date_ymd", "date_mdy", "date_dmy", "datetime_ymd", "datetime_mdy",
                    "datetime_dmy", "datetime_seconds_ymd", "datetime_seconds_mdy",
                    "datetime_seconds_dmy")
.dq_numeric_types <- c("integer", "number", "number_1dp", "number_2dp")

#' @rdname compute_implausibility
#'
#' @name compute_implausibility
#'
#' @title Compute out-of-range ("implausible") values per field
#'
#' @description For every field in \code{metadata} with a \code{text_validation_min} and/or
#' \code{text_validation_max} declared, flags values (among eligible, non-missing records)
#' that fall outside that range -- numeric fields are compared numerically, date fields as
#' dates. This is the implausibility computation used internally by
#' \code{\link{generate_data_quality_report}}; call it directly if you want the numbers
#' without writing an Excel file.
#'
#' @details Only records eligible per the field's branching logic, and not already coded
#' as missing (see \code{missing_codes}), are checked -- a blank field is a completeness
#' issue (see \code{\link{compute_completeness}}), not an implausibility issue. A non-blank
#' value that can't even be parsed as the field's declared type (e.g. text in a numeric
#' field) is flagged too, since that's just as much a data quality problem as being out of
#' range.
#'
#' @param data Raw REDCap data (data.frame or data.table), one row per record, column names
#' matching \code{metadata$field_name}
#' @param metadata REDCap metadata / data dictionary (data.frame or data.table)
#' @param missing_codes Character vector of raw values treated as coded missing data. See
#' \code{\link{compute_completeness}}
#' @param negative_char Checkbox expansion character for negative choice codes (unused here
#' directly, kept for a consistent signature with \code{\link{compute_completeness}})
#' @param max_detail_rows Safety cap on the number of rows returned in \code{detail} (default
#' 50,000); if exceeded, it is truncated and \code{truncated} is set to \code{TRUE}
#'
#' @return A list with: \code{summary} (a data.table, one row per range-validated field, with
#' \code{N_Checked}, \code{N_Implausible}, \code{Pct_Implausible}), \code{detail} (one row per
#' implausible value found, with \code{RecordID}, \code{Variable}, \code{Value}, \code{Reason}),
#' \code{logic_notes} (branching logic conditions that could not be evaluated, if any), and
#' \code{truncated} (logical, whether \code{detail} was capped by \code{max_detail_rows})
#'
#' @seealso \code{\link{generate_data_quality_report}}, \code{\link{compute_completeness}}
#'
#' @export compute_implausibility
#'
compute_implausibility <- function(data, metadata, missing_codes = c("-1", "-3", "Empty", "empty"),
                                   negative_char = "_", max_detail_rows = 50000) {
  setDT(data)
  meta <- copy(as.data.table(metadata))
  meta <- meta[tolower(field_type) != "descriptive"]
  meta[, key := .I]
  meta[, f.branching_logic := NA_character_]
  meta[str_trim(branching_logic) != "", f.branching_logic := convert_redcap2r(branching_logic), by = key]

  id_var <- names(meta)[1] %in% names(data)
  id_var <- if ("field_name" %in% names(meta) && nrow(meta) > 0 && meta$field_name[1] %in% names(data)) {
    meta$field_name[1]
  } else {
    NA_character_
  }

  ranged <- meta[str_trim(text_validation_min) != "" | str_trim(text_validation_max) != ""]
  notes_env <- new.env()

  summary_rows <- vector("list", nrow(ranged))
  detail_list <- vector("list", nrow(ranged))
  truncated <- FALSE
  detail_rows_so_far <- 0L

  for (i in seq_len(nrow(ranged))) {
    m <- ranged[i]
    fname <- m$field_name
    vtype <- tolower(str_trim(m$text_validation_type_or_show_slider_number))
    if (!fname %in% names(data)) next

    eligible <- .dq_eval_eligibility(m$f.branching_logic, data, notes_env)
    raw <- data[[fname]]
    miss <- .dq_is_missing(raw, missing_codes)
    checked <- eligible & !miss

    is_date <- vtype %in% .dq_date_types
    is_num <- vtype %in% .dq_numeric_types || (!is_date && (str_trim(m$text_validation_min) != "" | str_trim(m$text_validation_max) != ""))

    if (is_date) {
      val <- .dq_safe_as_date(raw)
      lo <- .dq_safe_as_date(str_trim(m$text_validation_min))
      hi <- .dq_safe_as_date(str_trim(m$text_validation_max))
    } else {
      val <- suppressWarnings(as.numeric(raw))
      lo <- suppressWarnings(as.numeric(str_trim(m$text_validation_min)))
      hi <- suppressWarnings(as.numeric(str_trim(m$text_validation_max)))
    }

    unparseable <- checked & is.na(val)
    below <- checked & !is.na(val) & !is.na(lo) & val < lo
    above <- checked & !is.na(val) & !is.na(hi) & val > hi
    bad <- (below | above) | (unparseable)

    n_checked <- sum(checked)
    n_bad <- sum(bad, na.rm = TRUE)

    summary_rows[[i]] <- data.table(
      Form = m$form_name,
      Variable = fname,
      Label = remove_html_tags(m$field_label),
      ValidationType = m$text_validation_type_or_show_slider_number,
      Min = if (is_date) as.character(lo) else lo,
      Max = if (is_date) as.character(hi) else hi,
      N_Checked = n_checked,
      N_Implausible = n_bad,
      Pct_Implausible = if (n_checked == 0) NA_real_ else round(100 * n_bad / n_checked, 1)
    )

    if (n_bad > 0 && detail_rows_so_far < max_detail_rows) {
      idx <- which(bad)
      if (detail_rows_so_far + length(idx) > max_detail_rows) {
        idx <- idx[seq_len(max_detail_rows - detail_rows_so_far)]
        truncated <- TRUE
      }
      detail_rows_so_far <- detail_rows_so_far + length(idx)
      detail_list[[i]] <- data.table(
        RecordID = if (!is.na(id_var)) data[[id_var]][idx] else idx,
        Form = m$form_name,
        Variable = fname,
        Label = remove_html_tags(m$field_label),
        Value = as.character(raw)[idx],
        Min = if (is_date) as.character(lo) else lo,
        Max = if (is_date) as.character(hi) else hi,
        Reason = ifelse(unparseable[idx], "Unparseable / not a valid value for its type",
                        ifelse(below[idx], "Below minimum", "Above maximum"))
      )
    }
  }

  summary <- rbindlist(summary_rows, use.names = TRUE)
  detail <- rbindlist(detail_list, use.names = TRUE)
  notes <- if (length(ls(notes_env)) == 0) {
    data.table(BranchingLogic = character(0), Note = character(0))
  } else {
    data.table(BranchingLogic = ls(notes_env), Note = unlist(mget(ls(notes_env), envir = notes_env)))
  }
  list(summary = summary, detail = detail, logic_notes = notes, truncated = truncated)
}

# ---------------------------------------------------------------------------
# Orchestrator: build & write the Excel workbook
# ---------------------------------------------------------------------------

#' @rdname generate_data_quality_report
#'
#' @name generate_data_quality_report
#'
#' @title Generate a Data Quality (completeness + implausibility) Excel report
#'
#' @description Builds an Excel workbook assessing the raw data against the REDCap data
#' dictionary: a Completeness sheet (missingness per field, with the denominator adjusted
#' for branching logic -- a field that only appears for a subset of records via branching
#' logic is only judged against that subset, not the whole dataset), and an Implausibility
#' sheet (values falling outside the field's declared \code{text_validation_min}/
#' \code{text_validation_max} range). Also accessible as the \code{get_data_quality_report()}
#' method on a \code{\link{Redcap}} object, which supplies \code{data}/\code{metadata}
#' automatically from the project's cache.
#'
#' @details All computation here is vectorized per FIELD across the full dataset (a handful
#' of vector operations per field), not per RECORD -- this is what keeps it fast even on
#' data dictionaries with hundreds of fields and datasets with many thousands of records.
#' See \code{\link{compute_completeness}} and \code{\link{compute_implausibility}} for the
#' underlying computations (both exported separately if you just want the data.tables
#' without writing an Excel file).
#'
#' Branching logic is evaluated once per field, across the whole dataset at once, using the
#' package's existing \code{convert_redcap2r()} translation. Any column referenced in a
#' branching-logic condition that looks numeric is coerced to numeric before evaluation, to
#' avoid lexicographic (string) comparison bugs (e.g. "9" > "100" is TRUE as strings but
#' FALSE as numbers). If a condition still cannot be evaluated (e.g. it references a field
#' not present in \code{data}), the field is conservatively treated as eligible for every
#' record (so a bad condition can only ever make completeness look *better* than it might
#' be flagged as unconditional, never silently hide records) and is listed on the "Notes"
#' sheet of the workbook.
#'
#' Checkbox fields are judged as a group: a checkbox field is "missing" for a record only if
#' every one of its choice sub-columns is unset (0/NA/blank) for that record -- i.e. the
#' respondent never touched the checkbox group. Any single choice selected counts the whole
#' group as answered for that record.
#'
#' The workbook has five sheets: \strong{Overview} (dataset size, average completeness,
#' counts), \strong{Completeness}, \strong{Implausibility Summary}, \strong{Implausibility
#' Detail} (one row per out-of-range value, capped by \code{max_detail_rows}), and
#' \strong{Notes} (branching logic conditions that couldn't be evaluated, if any).
#'
#' @param data Raw REDCap data (data.frame or data.table), one row per record, column names
#' matching \code{metadata$field_name} (checkbox fields already expanded to
#' \code{field___choice} columns, as REDCap exports them)
#' @param metadata REDCap metadata / data dictionary (data.frame or data.table)
#' @param output_path Path to write the .xlsx report to
#' @param missing_codes Character vector of raw values that should be treated as coded
#' missing data (in addition to \code{NA} and blank/whitespace-only strings). Defaults to
#' \code{c("-1", "-3", "Empty", "empty")}, REDCap's common "not recorded"/"not readable"
#' conventions
#' @param negative_char Checkbox expansion character for negative choice codes (matches the
#' rest of the package, e.g. \code{get_negative_char()} on a \code{Redcap} object)
#' @param max_detail_rows Safety cap on the number of rows written to the Implausibility
#' Detail sheet (default 50,000); if exceeded, the sheet is truncated and a note is added
#' @param open_after Logical. If TRUE, opens the generated file after writing
#'
#' @return (Invisibly) a list with the underlying \code{completeness}, \code{implausibility}
#' and \code{implausibility_detail} data.tables, in case the caller wants them directly
#' rather than the Excel file
#'
#' @seealso \code{\link{Redcap}} for the \code{get_data_quality_report()} method,
#' \code{\link{compute_completeness}}, \code{\link{compute_implausibility}}
#'
#' @examples
#' \dontrun{
#' generate_data_quality_report(
#'   data = redcap_obj$get_raw_data(),
#'   metadata = redcap_obj$get_metadata(),
#'   output_path = "data_quality_report.xlsx"
#' )
#' }
#'
#' @export generate_data_quality_report
#'
generate_data_quality_report <- function(data, metadata, output_path,
                                         missing_codes = c("-1", "-3", "Empty", "empty"),
                                         negative_char = "_", max_detail_rows = 50000,
                                         open_after = FALSE) {
  setDT(data)
  cat("Computing completeness...\n")
  comp <- compute_completeness(data, metadata, missing_codes, negative_char)
  cat("Computing implausibility...\n")
  impl <- compute_implausibility(data, metadata, missing_codes, negative_char, max_detail_rows)

  wb <- createWorkbook()

  hdr_style <- createStyle(textDecoration = "bold", fgFill = "#2C3E50", fontColour = "#FFFFFF",
                           halign = "center", wrapText = TRUE)
  pct_style <- createStyle(numFmt = "0.0")
  low_complete_style <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  has_implaus_style <- createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")

  # ---- Overview sheet ----
  addWorksheet(wb, "Overview")
  n_fields_checked_c <- nrow(comp$completeness[!is.na(N_Missing)])
  overview <- data.table(
    Metric = c(
      "Report generated", "Records in dataset", "Fields assessed for completeness",
      "Average completeness (%)", "Fields with any implausible values",
      "Total implausible values found", "Branching logic conditions that could not be evaluated"
    ),
    Value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M"),
      as.character(comp$n_total),
      as.character(n_fields_checked_c),
      as.character(round(mean(comp$completeness$Pct_Complete, na.rm = TRUE), 1)),
      as.character(sum(impl$summary$N_Implausible > 0, na.rm = TRUE)),
      as.character(sum(impl$summary$N_Implausible, na.rm = TRUE)),
      as.character(nrow(unique(rbindlist(list(comp$logic_notes, impl$logic_notes)))))
    )
  )
  writeData(wb, "Overview", overview, headerStyle = hdr_style)
  setColWidths(wb, "Overview", cols = 1:2, widths = c(48, 30))

  # ---- Completeness sheet ----
  addWorksheet(wb, "Completeness")
  writeData(wb, "Completeness", comp$completeness, headerStyle = hdr_style, withFilter = TRUE)
  freezePane(wb, "Completeness", firstRow = TRUE)
  pct_col <- which(names(comp$completeness) == "Pct_Complete")
  if (nrow(comp$completeness) > 0) {
    conditionalFormatting(wb, "Completeness", cols = pct_col, rows = 2:(nrow(comp$completeness) + 1),
                          rule = "<80", style = low_complete_style)
  }
  setColWidths(wb, "Completeness", cols = 1:ncol(comp$completeness), widths = "auto")

  # ---- Implausibility Summary sheet ----
  addWorksheet(wb, "Implausibility Summary")
  writeData(wb, "Implausibility Summary", impl$summary, headerStyle = hdr_style, withFilter = TRUE)
  freezePane(wb, "Implausibility Summary", firstRow = TRUE)
  if (nrow(impl$summary) > 0) {
    bad_col <- which(names(impl$summary) == "N_Implausible")
    conditionalFormatting(wb, "Implausibility Summary", cols = bad_col, rows = 2:(nrow(impl$summary) + 1),
                          rule = ">0", style = has_implaus_style)
  }
  setColWidths(wb, "Implausibility Summary", cols = 1:ncol(impl$summary), widths = "auto")

  # ---- Implausibility Detail sheet ----
  addWorksheet(wb, "Implausibility Detail")
  if (nrow(impl$detail) > 0) {
    writeData(wb, "Implausibility Detail", impl$detail, headerStyle = hdr_style, withFilter = TRUE)
    freezePane(wb, "Implausibility Detail", firstRow = TRUE)
    setColWidths(wb, "Implausibility Detail", cols = 1:ncol(impl$detail), widths = "auto")
    if (isTRUE(impl$truncated)) {
      writeData(wb, "Implausibility Detail",
                data.table(Note = paste0("Detail truncated at ", max_detail_rows, " rows; see Implausibility Summary for full counts.")),
                startRow = nrow(impl$detail) + 3)
    }
  } else {
    writeData(wb, "Implausibility Detail", data.table(Note = "No implausible values found."))
  }

  # ---- Notes sheet (fields whose branching logic couldn't be evaluated) ----
  all_notes <- unique(rbindlist(list(comp$logic_notes, impl$logic_notes)))
  addWorksheet(wb, "Notes")
  if (nrow(all_notes) > 0) {
    writeData(wb, "Notes", all_notes, headerStyle = hdr_style)
    setColWidths(wb, "Notes", cols = 1:2, widths = c(60, 60))
  } else {
    writeData(wb, "Notes", data.table(Note = "All branching logic conditions evaluated successfully."))
  }

  saveWorkbook(wb, output_path, overwrite = TRUE)
  cat("Data quality report written to:", output_path, "\n")
  if (open_after) open_using_default_app(output_path)

  invisible(list(completeness = comp$completeness, implausibility = impl$summary,
                 implausibility_detail = impl$detail))
}
