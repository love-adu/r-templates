# is the mean different from some number
t.test(dat$____, mu = ____)

# same thing split by group
dat |>
  group_by(____) |>
  summarise(n = n(), mean = mean(____, na.rm = TRUE), sd = sd(____, na.rm = TRUE),
            p = if (sum(!is.na(____)) >= 3) t.test(____, mu = ____)$p.value else NA_real_)
