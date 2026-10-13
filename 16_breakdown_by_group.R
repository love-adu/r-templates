# every question split by one grouping column
group_sheet <- function(gvar, drop = character()) {
  g <- dat[[gvar]]
  lv <- levels(droplevels(factor(g)))
  gn <- table(factor(g, levels = lv))
  rows <- meas |> filter(!(var %in% drop))

  out <- map_dfr(seq_len(nrow(rows)), function(i) {
    v <- rows$var[i]; x <- dat[[v]]
    means <- map_dbl(lv, ~ mean(x[which(g == .x)], na.rm = TRUE))
    ok <- !is.na(x) & !is.na(g)
    p <- NA_real_
    if (sum(ok) >= 10 && sd(x[ok]) > 0) {
      p <- if (length(lv) == 2) wilcox.test(x[ok] ~ factor(g[ok]), exact = FALSE)$p.value
           else kruskal.test(x[ok], factor(g[ok]))$p.value
    }
    row <- tibble(Question = rows$label[i])
    for (j in seq_along(lv)) row[[paste0(lv[j], " (n = ", gn[j], ")")]] <- round(means[j], 2)
    row[["Highest group"]] <- lv[which.max(means)]
    row[["Lowest group"]] <- lv[which.min(means)]
    row[["p"]] <- p
    row
  })
  out$q <- p.adjust(out$p, "BH")
  out
}

group_sheet("____", drop = c("____"))
