#' @import data.table
#' @import stringr
#' @import RCurl
#' @import Hmisc
#' @import methods
NULL

#' @name open_using_default_app
#'
#' @title Open file using  OS default application
#'
#' @description Pop a file open using the default app set at the operating system level to handle files of the type specified.
#'
#' @details This allows for results to be viewed interactively during analysis and programming.
#'
#' This is only applicable in GUI settings and should only be used in windowed environmnents.
#'
#' @param file_path Path to target file
#'
#' @export
#'

open_using_default_app = function(file_path) {
  file_path = paste0("\"", file_path, "\"")

  if(!file.exists(file_path))
    stop("The file does not exist!")
  if(Sys.info()["sysname"] == "Windows") {
    shell.exec(file_path)
  } else if (Sys.info()["sysname"] == "Linux") {
    tryCatch({
      system(paste("gnome-open", file_path, sep = " "), intern = FALSE)
    }
    , warning = function(w) warning(w$message)
    , error = function(e) stop(e$message)
    )
  }  else {
    tryCatch({
      system(paste0("open", file_path, sep = " "), intern = FALSE)
    }
    , warning = function(w) warning(w$message)
    , error = function(e) stop(e$message)
    )
  }
  Sys.sleep(2)
}
