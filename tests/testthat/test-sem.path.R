test_that(
  "'sem.path' works with cfa_fit, bif_fit, and extra",
  {
    bif_fit <- bif_fit[2]
    cfa_fit <- cfa_fit[1:2]
    path <- "grit_p ~ grit_c + hope"
    extra <- "grit_p ~~ hope_a + hope_p\ngrit_c ~~ hope_a + hope_p"
    fit <- sem.path(
      path, BFIGritHope, cfa_fit, bif_fit, extra = extra, fit_save = TRUE
    )

  }
)
