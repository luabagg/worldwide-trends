library(dplyr)
library(tibble)
library(scales)

source("utils.r")

output <- "out/daily-top-terms-classified.csv"

df <- read.csv(output) |>
  group_by(
    country, topic
  ) |>
  summarise(
    qty = n()
  ) |>
  arrange(
    topic
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

countries <- c("Brazil", "Portugal", "United Kingdom")

# Removing the column "country" for visualization.
chart_data <- df |>
  select(-country)
rownames(chart_data) <- df$country


pdf("out/analysis.pdf")

df_scaled <- round(apply(chart_data, 2, scales::rescale), 2)
df_scaled <- as.data.frame(df_scaled)
head(df_scaled)
col_max <- apply(df_scaled, 2, max)
col_min <- apply(df_scaled, 2, min)
# Calculate the average profile
col_mean <- apply(df_scaled, 2, mean)
col_sd <- apply(df_scaled, 2, sd)
# Put together the summary of columns
col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean, Sd = col_sd))
df_scaled2 <- as.data.frame(rbind(col_summary, df_scaled))
head(df_scaled2)

opar <- par()
# Define settings for plotting in a 3x4 grid, with appropriate margins:
par(mfrow = c(3, 2), mar = rep(1, 4))

# Produce a radar-chart for each student
for (i in 5:nrow(df_scaled2)) {
  fmsb::radarchart(
    df_scaled2[c(1:3, i), ],
    pfcol = c("#99999980", NA),
    pcol = c(NA, 2), plty = 1, plwd = 1,
    vlcex = 0.8,
    title = row.names(df_scaled2)[i]
  )
}
par <- par(opar)

# Close the graphics device
dev.off()

# png("out/comparing.png", width = 800, height = 600)
pdf("out/comparing.pdf")
par(mar = rep(1, 4))

countries <- c("Brazil", "Portugal", "United Kingdom")
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

colors <- c("#00AFBB", "#E7B800", "#FC4E07")

print(means)
fmsb::radarchart(
  comparing_df,
  axistype = 1,
  # Customize the polygon
  pcol = colors, pfcol = scales::alpha(colors, 0.5), plwd = 2, plty = 1,
  # Customize the grid
  cglcol = "grey", cglty = 1, cglwd = 1,
  # Customize the axis
  axislabcol = "grey",
  # Variable labels
  vlcex = 1,
  caxislabels = seq(min_values[1], max_values[1], length.out = 5), title = "Comparing countries",
)
par(mar = rep(0, 4))
legend(
  x = "bottom", legend = countries, horiz = TRUE,
  bty = "n", pch = 20, col = colors,
  text.col = "black", cex = 1, pt.cex = 1.5
)

par(opar)
dev.off()
