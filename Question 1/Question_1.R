# Data preparation for Question 1
# One-time setup: install.packages(c("dplyr", "readr"))


library(dplyr)
library(readr)

input_path <- "final_electricity_dataset.csv"
output_path <- "model_1_hourly.csv"

# Keep the quarter-hour observations and construct the variables required for
# the hourly Model 1 dataset.
quarter_hour_data <- read_csv(
  input_path,
  show_col_types = FALSE
)

# Rename the required columns by their stable source positions. This avoids
# depending on locale-specific rendering of the euro symbol in CSV headers.
names(quarter_hour_data)[c(1, 2, 3, 20, 22)] <- c(
  "start_time",
  "end_time",
  "price_de_eur_mwh",
  "repeated_interval",
  "residual_load_mwh"
)

quarter_hour_data <- quarter_hour_data |>
  mutate(
    # MWh during 15 minutes -> average GW: MWh / 0.25 hours / 1,000.
    residual_load_gw = residual_load_mwh / 250,
    delivery_date = as.Date(start_time, tz = "UTC"),
    delivery_hour = as.integer(format(start_time, "%H", tz = "UTC")),
    Post = if_else(
      delivery_date >= as.Date("2025-10-01"),
      1L,
      0L
    )
  )

# The autumn clock change repeats local timestamps. Including
# repeated_interval keeps the first and second occurrence as separate hours.
model_1_hourly <- quarter_hour_data |>
  group_by(
    delivery_date,
    delivery_hour,
    repeated_interval,
    Post
  ) |>
  summarise(
    price_de_eur_mwh = mean(price_de_eur_mwh, na.rm = TRUE),
    residual_load_gw = mean(residual_load_gw, na.rm = TRUE),
    quarters_in_hour = n(),
    .groups = "drop"
  ) |>
  mutate(
    residual_load_post = residual_load_gw * Post
  ) |>
  arrange(delivery_date, delivery_hour, repeated_interval)

# Every hourly observation must contain exactly four quarter-hours. The full
# two-year period contains 17,520 hours, including repeated autumn hours.
stopifnot(
  nrow(quarter_hour_data) == 70080L,
  nrow(model_1_hourly) == 17520L,
  all(model_1_hourly$quarters_in_hour == 4L)
)

write_csv(model_1_hourly, output_path, na = "")

message(
  "Saved Model 1 hourly data to: ",
  normalizePath(output_path, winslash = "/", mustWork = TRUE)
)

glimpse(model_1_hourly)
