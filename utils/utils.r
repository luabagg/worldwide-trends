#' This file contains utilitary functions.
#'
#' Should be called as following: source("utils.r").

#' get_requirements installs the packages from the given file.
#'
#' @param file the requirements file.
get_requirements <- function(file) {
  if (Sys.getenv("PACKAGES_INSTALLED") != "") {
    return()
  }

  if (!require("requiRements")) {
    install.packages("requiRements")
  }
  requiRements::install(NULL, file)

  Sys.setenv(PACKAGES_INSTALLED = TRUE)
}

#' print_m prints multiple lines at once.
#'
#' @param ... the lines to be printed (should be string).
print_m <- function(...) {
  cat(..., sep = "\n")
}

#' check_csv_filename checks if it's a valid csv filename
#'
#' @param filename
check_csv_filename <- function(filename) {
  ext <- stringr::str_split_i(filename, "[.]", i = -1)
  if (ext != "csv") {
    stop("The DAILY_TOP_TERMS_FILE must be a CSV file", ext)
  }
}

# Shared files in the scripts have their own retrieve function
#

#' daily_top_terms_file returns the defined filename
#' in wich the top terms will be stored.
daily_top_terms_file <- function(filename = Sys.getenv("DAILY_TOP_TERMS_FILE")) {
  if (filename == "") {
    filename <- "out/daily-top-terms.csv"
  }
  check_csv_filename(filename)

  return(filename)
}

#' classified_top_terms_file returns the defined filename
#' in wich the classified terms will be stored.
classified_top_terms_file <- function(filename = Sys.getenv("CLASSIFIED_TOP_TERMS_FILE")) {
  if (filename == "") {
    filename <- "out/daily-top-terms-classified.csv"
  }
  check_csv_filename(filename)

  return(filename)
}
