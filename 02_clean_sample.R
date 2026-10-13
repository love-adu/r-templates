# toss the id columns (ip, location, names, email)
dat <- raw |>
  select(-any_of(c("____", "____", "____"))) |>
  mutate(finished = str_to_lower(finished) == "true",
         progress = as.numeric(progress),
         duration = as.numeric(duration_in_seconds))

quality <- tibble(
  rows = nrow(dat),
  finished = sum(dat$finished),
  median_minutes = round(median(dat$duration, na.rm = TRUE) / 60, 1),
  speeders = sum(dat$duration < median(dat$duration, na.rm = TRUE) / 3, na.rm = TRUE),
  duplicate_ids = sum(duplicated(dat$responseid)))

# some people answered everything and just never hit the last button lol
keep_near_complete <- ____
dat_all <- dat
dat <- dat |> filter(finished | (keep_near_complete & progress >= ____))
