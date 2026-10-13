library(tidyverse)

data_path <- "____.csv"

# qualtrics has 3 header rows: names, question text, then junk ids
hdr <- names(read_csv(data_path, n_max = 0, show_col_types = FALSE))
raw <- read_csv(data_path, skip = 3, col_names = hdr,
                col_types = cols(.default = col_character()))
q_text <- read_csv(data_path, skip = 1, n_max = 1, col_names = hdr,
                   col_types = cols(.default = col_character())) |>
  pivot_longer(everything(), names_to = "variable", values_to = "question")

# column names are a mess so clean them
tidy_names <- function(x) {
  x |>
    str_replace_all("\u00a0", "") |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("_+$", "") |>
    str_to_lower()
}
names(raw) <- tidy_names(names(raw))
q_text$variable <- tidy_names(q_text$variable)

# only have the zip?
# csv_path <- unzip("____.zip", exdir = tempdir())[1]
