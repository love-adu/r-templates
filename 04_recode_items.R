# one line per block, copy paste as many as you need
dat <- dat |>
  mutate(
    across(starts_with("____"), ~ to_num(.x, scale_a), .names = "{.col}_n"),
    across(starts_with("____"), ~ to_num(.x, scale_b), .names = "{.col}_n"),
    ____ = to_num(____, c("____", "____", "____"), c(-1, 0, 1)),
    # the reversed ones, check these against your scoring key
    across(c(____, ____, ____), ~ rev_scale(.x, 1, ____)),
    # sliders and typed numbers
    across(c(____, ____), as.numeric),
    # yes/no to 0/1
    ____ = as.integer(____ == "____"))
