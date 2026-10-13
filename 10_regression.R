# z score so the betas can be compared
z <- function(x) as.numeric(scale(x))

# robust se because n is small
robust_tidy <- function(m) {
  ct <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC3"))
  tibble(term = rownames(ct), beta = ct[, 1], se = ct[, 2], p = ct[, 4]) |>
    mutate(lo = beta - qt(.975, m$df.residual) * se,
           hi = beta + qt(.975, m$df.residual) * se)
}

outcome <- "____"
preds <- c("____", "____", "____")
to_standardize <- c("____", "____")   # not the 0/1 ones

m <- dat |>
  select(all_of(c(outcome, preds))) |>
  drop_na() |>
  mutate(across(all_of(c(outcome, to_standardize)), z))

fit <- lm(reformulate(preds, outcome), data = m)

robust_tidy(fit)
c(r2 = summary(fit)$r.squared, adj_r2 = summary(fit)$adj.r.squared, n = nobs(fit))
