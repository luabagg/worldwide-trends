#' Worldwide Trends
#'
#' This script is responsible for analyzing the data.
#'
#' ----- ----- ----- ----- :) ----- ----- ----- -----

library(dplyr)
library(tibble)
library(scales)

source("utils/utils.r")


# Retrieving the classified terms.
#

output <- classified_top_terms_file()

df <- read.csv(output) |>
  group_by(
    country, topic
  ) |>
  summarise(
    qty = n()
  ) |>
  arrange(
    country
  ) |>
  tidyr::pivot_wider(
    names_from = topic, values_from = qty
  ) |>
  rename(
    "Uncategorized" = "NA"
  ) |>
  mutate_if(
    is.integer,
    ~ replace(., is.na(.), 0)
  ) |>
  as.data.frame() |>
  view()

opar <- par()

# Plotting all countries statistics
#

pdf("reports/radar-countries-complete.pdf")

title <- "Countries Trends Visualization \n (02/06 - 28/06)"

chart_data <- df |>
  select(-country)
rownames(chart_data) <- df$country

df_scaled <- round(
  apply(chart_data, 2, scales::rescale), 2
) |>
  as.data.frame()

# Calculating statistics
col_max <- apply(df_scaled, 2, max)
col_min <- apply(df_scaled, 2, min)
col_mean <- apply(df_scaled, 2, mean)
col_sd <- apply(df_scaled, 2, sd)

col_summary <- t(
  data.frame(
    Max = col_max, Min = col_min, Average = col_mean, Sd = col_sd
  )
)
max_min_mean_pos <- 1:3
statistics_cols_qty <- nrow(col_summary)

df_scaled <- as.data.frame(
  rbind(col_summary, df_scaled)
) |> view()


display_qty <- c(3, 2)
par(mfrow = display_qty, mar = rep(1, 4))

title_i <- prod(display_qty)
for (i in (statistics_cols_qty + 1):nrow(df_scaled)) {
  fmsb::radarchart(
    df_scaled[c(max_min_mean_pos, i), ],
    title = row.names(df_scaled)[i],
    vlcex = 0.8,
    pfcol = c("#99999980", NA),
    pcol = c(NA, 2),
    plty = 1,
  )

  if (title_i %% prod(display_qty) == 0) {
    mtext(
      title,
      side = 3, line = -2.3, cex = 0.7, col = "#505050", outer = TRUE
    )
  }
  title_i <- title_i + 1
}

par(opar)
dev.off()


# Plotting the PNG of countries comparison
#

png("reports/countries-comparison.png", width = 800, height = 600)
par(mar = rep(1, 4))

countries <- c("Brazil", "Canada", "New Zealand")
colors <- c("#00AFBB", "#FC4E07", "#E7B800")

comparing_df <- df |>
  filter(country %in% countries) |>
  select(-country)
rownames(comparing_df) <- countries
comparing_df_values <- comparing_df |> unlist()
comparing_df_cols <- comparing_df |> colnames()

max_min_df <- data.frame(matrix(ncol = length(comparing_df_cols), nrow = 2))
colnames(max_min_df) <- comparing_df_cols
rownames(max_min_df) <- c("Max", "Min")

max_values <- rep(
  max(comparing_df_values),
  ncol(comparing_df)
)
min_values <- rep(
  min(comparing_df_values),
  ncol(comparing_df)
)

max_min_df[1, ] <- max_values
max_min_df[2, ] <- min_values

comparing_df <- rbind(max_min_df, comparing_df)

fmsb::radarchart(
  comparing_df,
  title = "Comparing countries (02/06 - 28/06)",
  axistype = 1,
  caxislabels = paste(seq(min_values[1], max_values[1], length.out = 5), "terms"),
  vlcex = 1,
  pcol = colors,
  pfcol = scales::alpha(colors, 0.5),
  plwd = 2, plty = 1,
  cglcol = "grey", cglty = 1, cglwd = 1,
  axislabcol = "grey",
)
par(mar = rep(0, 4))
legend(
  x = "bottom", legend = countries, horiz = TRUE,
  bty = "n", pch = 20, col = colors,
  text.col = "black", cex = 1, pt.cex = 1.5
)

par(opar)
dev.off()
