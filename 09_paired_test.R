# same people rated two things
a <- dat$____
b <- dat$____

tt <- t.test(a, b, paired = TRUE)
wt <- wilcox.test(a, b, paired = TRUE, exact = FALSE)
dz <- mean(a - b, na.rm = TRUE) / sd(a - b, na.rm = TRUE)

tibble(mean_a = mean(a, na.rm = TRUE), mean_b = mean(b, na.rm = TRUE),
       diff = unname(tt$estimate), ci_low = tt$conf.int[1], ci_high = tt$conf.int[2],
       t = unname(tt$statistic), df = unname(tt$parameter),
       p_t = tt$p.value, p_wilcoxon = wt$p.value, dz = dz)
