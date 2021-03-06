% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_fields_without_ranges.R
\name{is_hidden}
\alias{is_hidden}
\title{Evaluate if the field name is hidden in REDCap data collection tool}
\usage{
is_hidden(b.logic, fieldname = NA_character_, metadata)
}
\arguments{
\item{fieldname}{Variable name}

\item{metadata}{REDCap metadata}

\item{b_logic}{Formatted branching logic associated with the variable name}
}
\value{
TRUE if the fieldname is hidden in REDCap
}
\description{
Determines if the fieldname is hidden based on branching logics and REDCap hidden functionality.
}
\seealso{
Other RedcapToR: \code{\link{convert_dates_red2r}},
  \code{\link{convert_missing_red2r}},
  \code{\link{format_branching_logics}},
  \code{\link{logical_xpressions_red2r}},
  \code{\link{xtend_chb_names}}
}
