#' get_months_range returns a range from x months ago.
#'
#' @param months integer representing how many months ago from now.
get_months_range <- function(months) {
  return(
    lubridate::today() - months(months)
  )
}

#' get_bq_connection returns the DBI BigQuery connection.
#'
#' @param project_id the user's project identifier.
#' @param project_name name of the BigQuery project.
#' @param dataset_name name of the BigQuery dataset
get_bq_connection <- function(project_id, project_name, dataset_name) {
  con <- (
    DBI::dbConnect(
      bigrquery::bigquery(),
      project = project_name,
      dataset = dataset_name,
      billing = project_id,
      pagesize = 20
    )
  )

  print("BigQuery connection:")
  print(con)

  return(con)
}

#' fmt_columns formats the columns in SQL format.
#'
#' @param ... the columns to be formatted.
fmt_columns <- function(...) {
  columns <- c(...)
  if (length(columns) == 0) {
    return("*")
  }
  return(
    stringr::str_c(columns, ", ")
  )
}

#' get_table returns the table from the given connection.
#'
#' @param con the DBI connection @see get_bq_connection.
#' @param table_name the table name to be retrieved.
get_table <- function(con, table_name) {
  tables <- DBI::dbListTables(con)
  for (table in tables) {
    if (table == table_name) {
      print(
        sprintf("Table found: %s", table_name)
      )
      return(
        dplyr::tbl(con, table_name)
      )
    }
  }

  stop("table not found in the connection")
}

#' write_complete_csv queries the given table and outputs to a file.
#'
#' warning: the given table must have the following columns:
#' - rank
#' - refresh_date
#' - week
#'
#' @param table the table @see get_table.
#' @param output the filename to output the info.
write_complete_csv <- function(table, output) {
  one_month_ago <- get_months_range(1)
  table |>
    dplyr::select(
      dplyr::everything()
    ) |>
    # Filters according to the defined timestamp (one month).
    # Retrieves the 5 top terms of each day.
    dplyr::filter(
      rank <= 5 &
        week >= one_month_ago &
        refresh_date >= one_month_ago
    ) |>
    utils::write.table(
      output,
      sep = ","
    )
}