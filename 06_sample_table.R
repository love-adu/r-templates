# counts and percents for a categorical column
pct_row <- function(df, col, label) {
  df |> count({{ col }}) |>
    transmute(variable = paste0(label, {{ col }}),
              stat = sprintf("%d (%.0f%%)", n, 100 * n / sum(n)))
}

# sample table
demo <- bind_rows(
  dat |> summarise(variable = "____",
    stat = sprintf("M = %.1f, SD = %.1f, range %d-%d", mean(____, na.rm = TRUE),
                   sd(____, na.rm = TRUE), min(____, na.rm = TRUE), max(____, na.rm = TRUE))),
  dat |> summarise(variable = "____",
    stat = sprintf("median %s", format(median(____, na.rm = TRUE), big.mark = ","))),
  pct_row(dat, ____, "____: "),
  pct_row(dat, ____, "____: "))
