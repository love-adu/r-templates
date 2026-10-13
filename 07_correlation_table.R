# every measure you want in the table
meas <- tibble(
  var = c("____", "____", "____"),
  label = c("____", "____", "____"),
  group = c("____", "____", "____"),
  kind = c("cont", "bin", "cont"))
k <- nrow(meas)

# spearman for every pair
rho <- p_m <- matrix(NA_real_, k, k, dimnames = list(meas$var, meas$var))
for (i in 1:(k - 1)) for (j in (i + 1):k) {
  x <- dat[[meas$var[i]]]; y <- dat[[meas$var[j]]]
  ok <- complete.cases(x, y)
  if (sum(ok) < 10 || sd(x[ok]) == 0 || sd(y[ok]) == 0) next
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  rho[i, j] <- rho[j, i] <- unname(ct$estimate)
  p_m[i, j] <- p_m[j, i] <- ct$p.value
}

# fix the p values for running a million tests
q_m <- matrix(NA_real_, k, k, dimnames = dimnames(p_m))
q_m[upper.tri(q_m)] <- p.adjust(p_m[upper.tri(p_m)], method = "BH")
q_m[lower.tri(q_m)] <- t(q_m)[lower.tri(q_m)]

stars <- function(p) case_when(is.na(p) ~ "", p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
r_fmt <- function(x) ifelse(is.na(x), "", sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x)))

# pairs that basically ask the same thing
overlap_pairs <- list(c("____", "____"), c("____", "____"))
ov_m <- matrix(FALSE, k, k, dimnames = dimnames(rho))
for (pr in overlap_pairs) ov_m[pr[1], pr[2]] <- ov_m[pr[2], pr[1]] <- TRUE

# text for each cell: rho, stars, dagger if it survives the fix, ~ if overlap, [ ] if two predictors are .6+
is_pred <- meas$group != "____"
cell_txt <- matrix("", k, k)
for (i in 1:k) for (j in 1:k) {
  if (i == j) { cell_txt[i, j] <- "-"; next }
  if (is.na(rho[i, j])) next
  tx <- paste0(r_fmt(rho[i, j]), stars(p_m[i, j]),
               ifelse(q_m[i, j] < .05, "\u2020", ""),
               ifelse(ov_m[i, j], "~", ""))
  if (is_pred[i] && is_pred[j] && abs(rho[i, j]) >= .6) tx <- paste0("[", tx, "]")
  cell_txt[i, j] <- tx
}
corr_out <- as_tibble(cell_txt, .name_repair = "minimal")
names(corr_out) <- as.character(seq_len(k))
corr_out <- bind_cols(Measure = paste(seq_len(k), meas$label), corr_out)
