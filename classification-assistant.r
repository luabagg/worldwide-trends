#' Worldwide Trends
#'
#' This script is responsible for classifying the
#' terms using OpenAI API.
#'
#' ----- ----- ----- ----- :) ----- ----- ----- -----


#'
#'
#' As at 30 Jun 2024, the OpenAI's assistants API is not fully working.
#' Vector stores are being saved with error.
#'
#'

source("utils.r")

get_requirements("./requirements.txt")
dotenv::load_dot_env(".Renviron")


# Retrieves grouped trends created in main.r
#

daily_top_terms_csv <- daily_top_terms_filename()
print_m(
  sprintf("Loading top terms %s...", daily_top_terms_csv)
)

if (!file.exists(daily_top_terms_csv)) {
  stop("you must first generate the output file (see readme.md)")
}

# Since OpenAI assistants API doesn't supports CSV yet,
# converts the data to TXT format.
grouped_terms_txt <- "out/grouped-top-terms.txt"
top_terms <- read.csv(daily_top_terms_csv) |>
  dplyr::group_by(
    term, country
  ) |>
  utils::write.table(
    grouped_terms_txt,
    sep = ",",
    col.names = FALSE,
    row.names = FALSE
  )


# Saves the dataset using OpenAI files API
#

print_m(
  sprintf("Saving dataset %s in OpenAI...", grouped_terms_txt)
)

file_obj <- NULL
filename <- stringr::str_split_i(grouped_terms_txt, "/", i = -1)

uploaded_files <- list_files_openai()
for (uploaded_file in uploaded_files$data) {
  # Extracts only filename, without folders.
  if (uploaded_file$filename == filename) {
    file_obj <- uploaded_file
    break
  }
}

if (is.null(file_obj)) {
  # If no file was found, uploads a new one.
  file_obj <- add_file_openai(
    grouped_terms_txt,
    "assistants"
  )
}

vector_store_obj <- create_vector_store_openai(
  filename,
  list(
    file_obj$id
  )
)


# Creates OpenAI assistant
#

print_m(
  "Creating new assistant..."
)

tools_cfg <- list(
  list(
    type = "file_search"
  )
)

assistant_obj <- create_assistant_openai(
  name = "Topics Categorization",
  instructions = "
    You excel at categorizing items. When presented a item,
    you will categorize with the most related topic
  ",
  tools = tools_cfg
)

print_m(
  sprintf("Assistant created, id %s", assistant_obj$id)
)

# Top trends classification by topics
#

topics <- c(
  "Politics",
  "Sports",
  "Economy",
  "Music",
  "Science & Technology",
  "Health & Medicine",
  "Environment",
  "Education",
  "Business & Finance",
  "Culture & Society",
  "Entertainment",
  "Travel & Tourism",
  "Food & Drink",
  "Lifestyle"
)

fmt_topics <- paste(paste("-", topics), collapse = "\n")
prompt <- paste(
  "There is a stored file containing trend searchs in TXT format.
  The TXT file contains daily trending terms from Google Trends grouped by countries.
  The file uses the following delimiters: <term> and <country>.
  Your job is to classify them with a set of topics from the following list.
  You MUST use localization according to each country.
  Provide your answer writing a new TXT file with the topic that categorizes each row.
  Choose ONLY one option from the list of topics provided here for each term:",
  fmt_topics,
  sep = "\n"
)

first_msg <- list(
  role = "user",
  content = prompt,
  attachments = list(
    list(
      file_id = file_obj$id,
      tools = tools_cfg
    )
  )
)

thread <- list(
  messages = list(
    first_msg
  )
)

print_m(
  "Initializing new run...",
  sprintf("Prompt: %s...", substr(prompt, 1, 300))
)

run_obj <- create_run_openai(
  assistant_obj$id,
  thread
)

print_m(
  sprintf("Run created, id %s", run_obj$id),
  "Awaiting messages..."
)

max_it <- 10
while (TRUE) {
  if (max_it == 0) {
    stop("could not complete run")
  }

  Sys.sleep(5)
  run_status <- get_run_openai(run_obj$thread_id, run_obj$id)
  if (
    run_status$status == "completed"
  ) {
    break
  }
  max_it <- max_it - 1
}

messages <- get_messages_openai(run_obj$thread_id)
for (msg in messages) {
  print(msg)
}
