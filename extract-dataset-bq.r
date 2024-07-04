#' Worldwide Trends
#'
#' This script is responsible for querying Google BigQuery
#' service and downloading / converting the datasets.
#'
#' ----- ----- ----- ----- :) ----- ----- ----- -----

source("utils/utils.r")
source("utils/big-query.r")

get_requirements("./requirements.txt")
dotenv::load_dot_env(".Renviron")

# Remember to setup .Renviron for this step.
project_id <- Sys.getenv("GCLOUD_PROJECT_ID")
project_name <- Sys.getenv("GCLOUD_PROJECT")
dataset_name <- Sys.getenv("GCLOUD_DATASET")
daily_top_terms_csv <- daily_top_terms_file()

print_m(
  sprintf("Your project ID: %s", project_id),
  sprintf("Your dataset: %s.%s", project_name, dataset_name),
  sprintf("The output file: %s", daily_top_terms_csv)
)

# Connects to the BigQuery using the configured project ID.
con <- get_bq_connection(project_id, project_name, dataset_name)

library(dplyr)

#' get_daily_top_terms returns the top terms
#' of each day, grouped by contry.
#'
#' warning: the given df must have the following columns:
#' - country_name
#' - refresh_date
#' - term
#' - rank
#'
#' @param table the table @see get_table.
#' @param output the filename to output the info.
get_daily_top_terms <- function(df) {
  # Top terms dataset (US and Worldwide):
  #
  # Rank (1, 2, 3...) is the rank of each term by day.
  # Score is the percentage of interest of each term compared to itself.

  daily_top_terms <- df |>
    select(
      country_name, refresh_date, term, rank
    ) |>
    rename(
      country = country_name,
      day = refresh_date
    ) |>
    group_by(
      # Groups the terms by its countries.
      country, day, term, rank
    ) |>
    summarise(
      # repetitions is the quantity of times that the term is present;
      repetitions = n()
    )

  return(daily_top_terms)
}


# Worldwide filtering
#

top_terms_complete_csv <- "out/top-terms-complete.csv"
if (!file.exists(top_terms_complete_csv)) {
  # If file doesn't exists, query the defined dataset and create csv.
  write_complete_csv(
    get_table(con, "international_top_terms"),
    top_terms_complete_csv
  )
}

print_m(
  sprintf("Loading dataset %s...", top_terms_complete_csv)
)

top_terms_complete_df <- read.csv(top_terms_complete_csv)
worldwide_top_terms <- get_daily_top_terms(top_terms_complete_df)


# US only filtering
#

us_top_terms_complete_csv <- "out/us-top-terms-complete.csv"
if (!file.exists(us_top_terms_complete_csv)) {
  # If file doesn't exists, query the defined dataset and create csv.
  write_complete_csv(
    get_table(con, "top_terms"),
    us_top_terms_complete_csv
  )
}

print_m(
  sprintf("Loading dataset %s...", us_top_terms_complete_csv)
)

us_top_terms_df <- read.csv(us_top_terms_complete_csv) |>
  mutate(
    # Add column to normalize US dataset with worldwide dataset.
    country_name = "United States of America"
  )
us_top_terms <- get_daily_top_terms(us_top_terms_df)


# Merging datasets
#

daily_top_terms <- merge(
  worldwide_top_terms,
  us_top_terms,
  all = TRUE
) |>
  arrange(
    country, day, rank
  )

daily_top_terms |>
  as.data.frame() |>
  sample_n(10) |>
  tibble::view(title = "Top Terms Sample")

print_m(
  sprintf("Writing to output %s...", daily_top_terms_csv)
)

daily_top_terms |> utils::write.table(
  daily_top_terms_csv,
  sep = ","
)
