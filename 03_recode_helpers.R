# turns "strongly agree" into 7 etc, yells if a label doesnt match
to_num <- function(x, levels, values = seq_along(levels)) {
  key <- str_to_lower(str_squish(x))
  out <- values[match(key, levels)]
  bad <- !is.na(x) & is.na(out)
  if (any(bad)) warning("labels not matched: ", paste(unique(x[bad]), collapse = " | "))
  out
}

# flips an item so a 7 becomes a 1
rev_scale <- function(x, lo, hi) (hi + lo) - x

# answer labels in order, lowest first, all lowercase
scale_a <- c("____", "____", "____", "____", "____", "____", "____")
scale_b <- c("____", "____", "____", "____", "____")
