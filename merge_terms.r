#' Worldwide Trends
#'
#' This script is responsible for merging
#' the datasets generated in main.r and classification.r.
#'
#' ----- ----- ----- ----- :) ----- ----- ----- -----

source("utils.r")

get_requirements("./requirements.txt")
dotenv::load_dot_env(".Renviron")


# Retrieving grouped trends created in main.r
#

daily_top_terms_csv <- daily_top_terms_filename()
classified_terms_csv <- classified_terms_filename()

print_m(
  sprintf("Loading %s and %s...", daily_top_terms_csv, classified_terms_csv)
)

if (!file.exists(daily_top_terms_csv) || !file.exists(classified_terms_csv)) {
  stop("you must first generate the output file (see readme.md)")
}

all_trending_terms <- read.csv(daily_top_terms_csv)
classified_terms <- read.csv(classified_terms_csv)


# Merging dataframes
#

library(dplyr)
library(stringr)

output <- "out/daily-top-terms-classified.csv"
print_m(
  sprintf("Merging dataframes to output %s...", output)
)

complete_terms <- merge(
  all_trending_terms,
  classified_terms,
  by = c("term", "country"),
  all.x = TRUE
) |>
  group_by(
    country, term, day, rank, repetitions
  ) |>
  summarise(
    topic = first(topic)
  ) |>
  arrange(
    country, day, rank
  ) |>
  utils::write.table(
    output,
    sep = ","
  )

print_m("All done!")