# who skipped stuff (finished people only)
item_cols <- names(raw)[which(names(raw) == "____"):ncol(raw)]
fin_rows <- raw[str_to_lower(raw$finished) == "true", item_cols]
miss <- tibble(item = item_cols,
               pct_missing = map_dbl(fin_rows, ~ round(100 * mean(is.na(.x)), 1))) |>
  arrange(desc(pct_missing))

# did they pass the attention check
attn_pass <- sum(dat$____ == ____, na.rm = TRUE)

# same answer all the way down a block
blocks <- list(____ = paste0("____", 1:____), ____ = c("____", "____"))
straight <- map_dfr(names(blocks), function(b) {
  m <- dat[, blocks[[b]]]
  same <- apply(m, 1, function(r) n_distinct(str_to_lower(r)) == 1)
  tibble(block = b, items = ncol(m), n_identical = sum(same),
         pct_identical = round(100 * mean(same), 1))
})

# everybody picked the top or the bottom
fc <- map_dfr(c("____", "____"), function(v) {
  x <- dat[[v]]
  tibble(item = v,
         pct_floor = round(100 * mean(x == ____, na.rm = TRUE), 1),
         pct_ceiling = round(100 * mean(x == ____, na.rm = TRUE), 1),
         pct_modal = round(100 * max(table(x)) / sum(!is.na(x)), 1))
}) |> mutate(flag = pct_floor >= 70 | pct_ceiling >= 70 | pct_modal >= 75)

# items that dont go with the rest of their scale
item_tot <- map_dfr(names(scale_defs), function(nm) {
  items <- scale_defs[[nm]]
  a <- suppressWarnings(suppressMessages(
    psych::alpha(dat[, items], check.keys = FALSE, warnings = FALSE)))
  tibble(scale = nm, item = items, r_drop = round(a$item.stats$r.drop, 2))
}) |> mutate(weak_item = r_drop < .30)

# answers that cant be real (two percents that add past 100 lol)
over100 <- dat |> filter(____ + ____ > 100)

# wording problems
qflags <- q_text |>
  filter(variable %in% names(dat)) |>
  mutate(asterisk = str_detect(question, "\\*\\s*$"),
         previous = str_detect(str_to_lower(question), "____"))

# answer options nobody picked
used <- unique(str_to_lower(str_squish(unlist(dat[, c("____", "____")]))))
setdiff(c("____", "____", "____"), used)

# flags table, add a line for each thing that fires
flag_rows <- list()
add_flag <- function(level, flag, where, detail, count) {
  flag_rows[[length(flag_rows) + 1]] <<- tibble(
    Level = level, Flag = flag, Where = where, Detail = detail, Count = as.character(count))
}
if (nrow(over100) > 0) add_flag("Problem", "____", "____", "____", nrow(over100))
if (any(fc$flag)) add_flag("Check", "____", "____", "____", sum(fc$flag))
flags_tbl <- bind_rows(flag_rows)
