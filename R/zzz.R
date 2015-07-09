.onLoad <- function(...) {
  
}

.onAttach <- function(...) {
  suppressWarnings(suppressPackageStartupMessages(library(RCurl)))
  suppressWarnings(suppressPackageStartupMessages(library(stringr)))
  suppressWarnings(suppressPackageStartupMessages(library(data.table)))
  packageStartupMessage(paste0(
    c(
      "A data management utility package to interface with REDCap data.",
      paste0(
        "See ", dQuote("https://github.com/bonmac/RedcapData"), " for more details."
      )
    ), collapse = "\n"
  ))
}