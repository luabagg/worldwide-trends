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

#' get_months_range returns a range from x months ago.
#'
#' @param months integer representing how many months ago from now.
get_months_range <- function(months) {
  return(
    lubridate::today() - months(months)
  )
}

#' daily_top_terms_filename returns the defined filename
#' in wich the top terms will be stored.
daily_top_terms_filename <- function() {
  daily_top_terms <- Sys.getenv("DAILY_TOP_TERMS_FILE")

  if (daily_top_terms == "") {
    daily_top_terms <- "out/daily-top-terms.csv"
  }

  ext <- stringr::str_split_i(daily_top_terms, "[.]", i = -1)
  if (ext != "csv") {
    stop("The DAILY_TOP_TERMS_FILE must be a CSV file", ext)
  }

  return(daily_top_terms)
}

#' classified_terms_filename returns the defined filename
#' in wich the top terms will be stored.
classified_terms_filename <- function() {
  classified_terms <- Sys.getenv("CLASSIFIED_TERMS_FILE")

  if (classified_terms == "") {
    classified_terms <- "out/classified-terms.csv"
  }

  ext <- stringr::str_split_i(classified_terms, "[.]", i = -1)
  if (ext != "csv") {
    stop("The CLASSIFIED_TERMS_FILE must be a CSV file", ext)
  }

  return(classified_terms)
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

# OpenAI Requests

get_api_key <- function() {
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (openai_api_key == "") {
    stop("OpenAI API key not found. Check your .Renviron file.")
  }

  return(openai_api_key)
}

make_request <- function(
  method = c("GET", "POST", "PUT", "DELETE"), url, headers, body = NULL
) {
  requests <- list(
    "GET" = httr::GET,
    "POST" = httr::POST,
    "PUT" = httr::PUT,
    "DELETE" = httr::DELETE
  )
  method <- match.arg(method)

  httr_method <- requests[[method]]
  res <- httr_method(
    url = url,
    httr::add_headers(headers),
    body = body
  )

  if (httr::status_code(res) != 200) {
    stop(
      "error requesting OpenAI API: ",
      httr::content(res)
    )
  }

  return(
    httr::content(res)
  )
}

chat_completition_openai <- function(..., model = "gpt-4o", api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )
  body <- rjson::toJSON(
    list(
      messages = list(...),
      model = model,
      n = 1,
      response_format = list(
        type = "json_object"
      )
    )
  )

  return(make_request(
    "POST",
    "https://api.openai.com/v1/chat/completions",
    headers,
    body
  ))
}

list_files_openai <- function(api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  return(make_request(
    "GET",
    "https://api.openai.com/v1/files",
    headers
  ))
}

add_file_openai <- function(filepath, purpose, api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "multipart/form-data"
  )
  body <- list(
    file = httr::upload_file(filepath),
    purpose = purpose
  )

  return(make_request(
    "POST",
    "https://api.openai.com/v1/files",
    headers,
    body
  ))
}

create_vector_store_openai <- function(name, file_ids, api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json",
    `OpenAI-Beta` = "assistants=v2"
  )
  body <- rjson::toJSON(
    list(
      file_ids = file_ids,
      name = name,
      expires_after = list(
        days = 1,
        anchor = "last_active_at"
      )
    )
  )

  return(make_request(
    "POST",
    "https://api.openai.com/v1/vector_stores",
    headers,
    body
  ))
}

create_assistant_openai <- function(name, instructions, tools, model = "gpt-3.5-turbo", api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json",
    `OpenAI-Beta` = "assistants=v2"
  )
  body <- rjson::toJSON(
    list(
      name = name,
      instructions = instructions,
      tools = tools,
      model = model
    )
  )

  return(make_request(
    "POST",
    "https://api.openai.com/v1/assistants",
    headers,
    body
  ))
}

create_run_openai <- function(assistant_id, thread, api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json",
    `OpenAI-Beta` = "assistants=v2"
  )
  body <- rjson::toJSON(
    list(
      assistant_id = assistant_id,
      thread = thread
    )
  )

  return(make_request(
    "POST",
    "https://api.openai.com/v1/threads/runs",
    headers,
    body
  ))
}

get_run_openai <- function(thread_id, run_id, api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json",
    `OpenAI-Beta` = "assistants=v2"
  )

  return(make_request(
    "GET",
    sprintf("https://api.openai.com/v1/threads/%s/runs/%s", thread_id, run_id),
    headers
  ))
}

get_messages_openai <- function(thread_id, api_key = get_api_key()) {
  headers <- c(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json",
    `OpenAI-Beta` = "assistants=v2"
  )

  return(make_request(
    "GET",
    sprintf("https://api.openai.com/v1/threads/%s/messages", thread_id),
    headers
  ))
}
