#' Worldwide Trends
#'
#' This script is responsible for classifying the
#' terms using OpenAI API.
#'
#' ----- ----- ----- ----- :) ----- ----- ----- -----

source("utils.r")

get_requirements("./requirements.txt")
dotenv::load_dot_env(".Renviron")


# Retrieving grouped trends created in main.r
#

daily_top_terms_csv <- daily_top_terms_filename()
print_m(
  sprintf("Loading top terms %s...", daily_top_terms_csv)
)

if (!file.exists(daily_top_terms_csv)) {
  stop("you must first generate the output file (see readme.md)")
}


# Defining the topics and trending terms
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

library(dplyr)

# Parses the daily top terms to decrease number of rows.
all_trending_terms <- read.csv(daily_top_terms_csv) |>
  select(
    term, country
  ) |>
  rename(
    term_name = term, country_name = country
  ) |>
  group_by(
    term_name, country_name
  ) |>
  summarise() |>
  arrange(
    country_name, term_name
  ) |>
  as.data.frame()

total_subsets <- ceiling(nrow(all_trending_terms) / 80)

# Split the data frame in total_subsets to fit OpenAI's rate limit.
tt_subsets <- split(
  all_trending_terms, factor(sort(rank(row.names(all_trending_terms)) %% total_subsets))
)

#' classify requests the terms classification.
#'
#' @param trending_terms dataframe of trending terms.
#' @param topics list of topics.
classify <- function(trending_terms, topics) {
  fmt_topics <- paste(paste("-", topics), collapse = "\n")
  fmt_terms <- rjson::toJSON(trending_terms)

  sample_size <- 5
  if (nrow(trending_terms) < sample_size) {
    sample_size <- nrow(trending_terms)
  }

  print_m(
    "Starting classification with OpenAI...",
    "Topics:",
    fmt_topics,
    sprintf("Terms: %s...", dplyr::sample_n(trending_terms |> select(term_name), sample_size))
  )

  prompt <- paste(
    "
    Below, you will be presented with a list of topics and a JSON of trending terms.
    The topics are delimited with '### Topics' and the trending terms are delimited with '### Terms'.
    The trending terms are from Google Trends and are grouped by countries,
    with the following format: [{`term_name`: term, `country_name`: country}, ...]).
    Your job is to classify them with the set of topics appropriately.
    Provide your answer with a JSON containing the topic that categorizes each row,
    and return the received JSON grouped by the topic
    using the following format: `topic_name`: [`received_JSON`, ...],
    in which the `topic_name` MUST be EXACTLY EQUAL to one in the topics list
    and the `received_JSON` MUST be EXACTLY EQUAL to the one in the terms list.
    You MUST use localization according to each country.
    You MUST only use the topics and terms provided here.
    Do NOT modify any topic or term.
    Do NOT wrap the JSON codes in JSON markers.
    Group ALL the terms with its respective topic.
    Analyze ALL the terms provided.
    Choose ONLY one option from the list of topics provided here for each term:
    ",
    "### Topics", fmt_topics, "### Topics",
    "### Terms", fmt_terms, "### Terms",
    sep = "\n"
  )

  print_m(
    "Setting up messages...",
    sprintf("Prompt: %s...", substr(prompt, 1, 300)),
    "Requesting chat completition..."
  )

  res <- chat_completition_openai(
    list(
      role = "system",
      content = "You're a helpful assistant that excels at categorizing trending terms.
      When presented a term, you will categorize with the most related topic."
    ),
    list(
      role = "user",
      content = prompt
    )
  )

  print_m(
    "Done!",
    "Parsing response..."
  )

  assistant_msg <- rjson::fromJSON(
    res$choices[[1]]$message$content
  )

  classified_terms <- data.frame(term = character(), country = character(), topic = character())
  for (topic in names(assistant_msg)) {
    rows <- assistant_msg[[topic]]
    for (row in rows) {
      classified_terms <- classified_terms |>
        dplyr::add_row(
          term = row$term_name,
          country = row$country_name,
          topic = topic
        )
    }
  }

  print_m("Response parsed!")

  return(classified_terms)
}

#' exec_classification executes the classify function with fallback.
#'
#' @param trending_terms dataframe of trending terms.
#' @param topics list of topics.
exec_classification <- function(tt, topics, it, n) {
  max_retry <- 3
  tryCatch(
    {
      classification <- classify(tt, topics)
    },
    error = function(e) {
      print_m(
        sprintf("error in iteration %s", it)
      )
      print(e)

      if (n <= max_retry) {
        exec_classification(tt, topics, it, n + 1)
      }
    }
  )

  return(classification)
}

# Requesting OpenAI
#

classified_terms_csv <- classified_terms_filename()
it <- 1
start_time <- Sys.time()


for (tt in tt_subsets) {
  classification <- exec_classification(tt, topics, it, 1)
  if (it == 1) {
    classification |>
      utils::write.table(
        classified_terms_csv,
        row.names = FALSE,
        sep = ","
      )
  } else {
    classification |>
      utils::write.table(
        classified_terms_csv,
        sep = ",",
        col.names = FALSE,
        row.names = FALSE,
        append = TRUE
      )
  }

  print_m(
    "\n---------",
    sprintf("Iteration %s of %s completed", it, nrow(tt_subsets)),
    "---------\n"
  )
  it <- it + 1
}

# Add row names and removes trailing whitespace.
read.csv(classified_terms_csv) |>
  mutate(
    across(
      where(is.character), stringr::str_trim
    ),
    topic = ifelse(match(topic, topics), topic, NA)
  ) |>
  utils::write.table(
    classified_terms_csv,
    sep = ","
  )

print_m(
  sprintf("Analysis completed, time spent: %s seconds", round(Sys.time() - start_time)),
  sprintf("Output available at %s", classified_terms_csv)
)
