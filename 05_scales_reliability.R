# which items go in which scale
scale_defs <- list(
  ____ = paste0("____", 1:____, "_n"),
  ____ = paste0("____", 1:____, "_n"),
  ____ = c("____", "____"))

# average them and get alpha
rel_tbl <- map_dfr(names(scale_defs), function(nm) {
  x <- dat[, scale_defs[[nm]]]
  a <- suppressWarnings(suppressMessages(
    psych::alpha(x, check.keys = FALSE, warnings = FALSE)))
  dat[[nm]] <<- rowMeans(x)
  tibble(scale = nm, items = ncol(x), n = sum(complete.cases(x)),
         alpha = round(a$total$raw_alpha, 2), mean_r = round(a$total$average_r, 2))
})

# scores built from other scores (flipping sdo into egalitarian, groups, etc)
dat <- dat |>
  mutate(____ = 8 - ____,
         ____ = ____ - ____,
         ____ = factor(case_when(____ < -0.5 ~ "____",
                                 ____ > 0.5 ~ "____",
                                 TRUE ~ "____"),
                       levels = c("____", "____", "____")))
