#' This file contains the requests to OpenAI API.
#'
#' Should be called as following: source("openai-api.r").

#' get_api_key gets the defined API Key
get_api_key <- function(openai_api_key = Sys.getenv("OPENAI_API_KEY")) {
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
