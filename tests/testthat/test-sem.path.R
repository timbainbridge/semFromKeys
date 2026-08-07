test_that(
  "'sem.path' works with cfa_fit, bif_fit, and extra",
  {
    path <- "grit_p ~ grit_c + hope_a\nhope_p ~ hope_a"
    extra <- "grit_c ~~ hope_a + hope_p"
    sem_fit <-
      sem.path(path, BFIGritHope, cfa_fit, extra = extra, fit_save = TRUE)
    expect_equal(length(sem_fit), 4)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_equal(length(sem_fit$b), 2)
    expect_equal(nrow(sem_fit$r2), length(keys))
  }
)
