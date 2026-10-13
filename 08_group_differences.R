group_var <- "____"
outcome <- "____"

# two groups = wilcoxon, 3 or more = kruskal
dd <- tibble(grp = dat[[group_var]], y = dat[[outcome]]) |> drop_na()
p <- if (n_distinct(dd$grp) == 2) {
  wilcox.test(y ~ grp, data = dd, exact = FALSE)$p.value
} else {
  kruskal.test(y ~ grp, data = dd)$p.value
}
dd |>
  group_by(grp) |>
  summarise(n = n(), mean = mean(y), sd = sd(y)) |>
  mutate(p = p)
