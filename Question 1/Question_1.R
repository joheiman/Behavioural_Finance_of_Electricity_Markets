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

# Add calendar variables used by the fixed-effects specifications.
model_1_hourly <- model_1_hourly |>
  mutate(
    month = factor(format(delivery_date, "%m")),
    day_of_week_number = as.integer(format(delivery_date, "%u")),
    day_of_week = factor(
      day_of_week_number,
      levels = 1:7,
      labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    hour_of_day = factor(delivery_hour)
  )

# Specification A - No fixed effects
model_a <- lm(
  price_de_eur_mwh ~ residual_load_gw + residual_load_post,
  data = model_1_hourly
)

# Specification B - Date fixed effects
model_b <- lm(
  price_de_eur_mwh ~ residual_load_gw + residual_load_post +
    factor(delivery_date),
  data = model_1_hourly
)

# Specification C - Date and hour-of-day fixed effects
model_c <- lm(
  price_de_eur_mwh ~ residual_load_gw + residual_load_post +
    factor(delivery_date) + hour_of_day,
  data = model_1_hourly
)

# Specification D - Month, weekday (Monday-Sunday), and hour-of-day fixed effects
model_d <- lm(
  price_de_eur_mwh ~ residual_load_gw + residual_load_post +
    month + day_of_week + hour_of_day,
  data = filter(model_1_hourly, day_of_week_number %in% 1:7)
)

# Write all model summaries and post-reform slopes to a text file.
models <- list(A = model_a, B = model_b, C = model_c, D = model_d)
model_output_path <- "question_1_model_output.txt"

capture.output(
  {
    cat("Question 1 regression results\n")
    cat("Generated:", format(Sys.time()), "\n")
    cat("Total hourly observations:", nrow(model_1_hourly), "\n\n")

    for (specification in names(models)) {
      fitted_model <- models[[specification]]
      coefficients <- coef(fitted_model)
      beta_post <- unname(
        coefficients["residual_load_gw"] + coefficients["residual_load_post"]
      )

      cat(paste0(strrep("=", 72), "\n"))
      cat("Specification", specification, "\n")
      cat(paste0(strrep("=", 72), "\n\n"))
      print(summary(fitted_model))
      cat("\nPost-reform slope (beta_1 + beta_2):", beta_post, "\n\n")
    }
  },
  file = model_output_path
)

message(
  "Saved all regression output to: ",
  normalizePath(model_output_path, winslash = "/", mustWork = TRUE)
)
