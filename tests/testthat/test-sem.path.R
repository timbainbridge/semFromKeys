test_that(
  "'sem.path' works with cfa_fit, bif_fit, and extra",
  {
    path <- "grit_p ~ grit_c + hope_a + hope_p"
    extra <- "grit_c ~~ hope_a + hope_p\nhope_a ~~ hope_p"
    fit <- sem.path(path, BFIGritHope, cfa_fit, extra = extra, fit_save = TRUE)

  }
)
