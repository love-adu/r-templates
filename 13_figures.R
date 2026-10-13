library(ggplot2)
theme_set(theme_minimal(base_size = 10))

# counts
p <- ggplot(____, aes(____, n)) +
  geom_col(fill = "#4c78a8") +
  geom_text(aes(label = n), vjust = -.3) +
  facet_wrap(~ ____, scales = "free_x") +
  labs(x = NULL, y = "people")

# oppose / neither / support bars
p <- ggplot(____, aes(pct, ____, fill = ____)) +
  geom_col() +
  scale_fill_manual(values = c(____ = "#b2182b", ____ = "grey65", ____ = "#2166ac")) +
  labs(x = "% of respondents", y = NULL, fill = NULL)

# heatmap
p <- ggplot(____, aes(____, ____, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ____), size = 2.4) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", limits = c(-1, 1))

# scatter with a line
p <- ggplot(dat, aes(____, ____)) +
  geom_point(alpha = .6, color = "#4c78a8") +
  geom_smooth(method = "lm", color = "firebrick", formula = y ~ x) +
  labs(x = "____", y = "____")

# boxplot by group
p <- ggplot(dat, aes(____, ____, fill = ____)) +
  geom_boxplot(alpha = .4, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = .12, alpha = .6, show.legend = FALSE) +
  labs(x = NULL, y = "____")

# same people, two ratings
p <- dat |>
  select(responseid, ____, ____) |>
  pivot_longer(-responseid) |>
  ggplot(aes(name, value)) +
  geom_line(aes(group = responseid), alpha = .2) +
  geom_jitter(width = .05, height = .08, alpha = .5) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "firebrick")

# regression coefficients
p <- ggplot(____, aes(beta, ____, xmin = lo, xmax = hi)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange() +
  labs(x = "standardized beta, 95% CI", y = NULL)

# dot plot
p <- ggplot(____, aes(pct, reorder(____, pct))) +
  geom_point(size = 2) +
  geom_vline(xintercept = ____, linetype = 2)

# save it
ggsave("____.png", p, width = ____, height = ____, dpi = 300)
