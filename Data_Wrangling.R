# One-time setup:
# install.packages(c("dplyr", "readr"))
library(dplyr)
library(readr)

get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- command_args[grepl("^--file=", command_args)]

  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }

  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()
  ) {
    script_path <- rstudioapi::getActiveDocumentContext()$path

    if (nzchar(script_path)) {
      return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
    }
  }

  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile[[1]])
    },
    character(1)
  )
  frame_files <- frame_files[!is.na(frame_files)]

  if (length(frame_files) > 0) {
    script_path <- frame_files[[length(frame_files)]]
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }

  getwd()
}

project_dir <- get_script_dir()
data_dir <- file.path(project_dir, "csv_exports")

day_ahead_prices <- read_csv(
  file.path(data_dir, "Data__Day-ahead prices.csv"),
  show_col_types = FALSE
)

forecasted_consumption <- read_csv(
  file.path(data_dir, "Data__Forecasted consumption.csv"),
  show_col_types = FALSE
)

forecasted_generation <- read_csv(
  file.path(data_dir, "Data__Forecasted generation DA.csv"),
  show_col_types = FALSE
)

join_columns <- c("Start date", "End date")

add_interval_occurrence <- function(dataset) {
  dataset |>
    group_by(across(all_of(join_columns))) |>
    # The autumn clock change repeats local intervals. Number repeated
    # timestamp pairs so their first and second occurrences remain distinct.
    mutate(interval_occurrence = row_number()) |>
    ungroup()
}

day_ahead_prices <- day_ahead_prices |>
  add_interval_occurrence()

forecasted_consumption <- forecasted_consumption |>
  add_interval_occurrence()

forecasted_generation <- forecasted_generation |>
  add_interval_occurrence()

join_columns <- c(join_columns, "interval_occurrence")

expected_rows <- nrow(day_ahead_prices)

stopifnot(
  nrow(forecasted_consumption) == expected_rows,
  nrow(forecasted_generation) == expected_rows
)

data <- day_ahead_prices |>
  full_join(
    forecasted_consumption,
    by = join_columns,
    relationship = "one-to-one"
  ) |>
  full_join(
    forecasted_generation,
    by = join_columns,
    relationship = "one-to-one"
  ) |>
  arrange(`Start date`, interval_occurrence)

stopifnot(nrow(data) == expected_rows)

glimpse(data)

output_path <- file.path(project_dir, "final_electricity_dataset.csv")
write_csv(data, output_path)

message(
  "Saved final dataset to: ",
  normalizePath(output_path, winslash = "/", mustWork = TRUE)
)
