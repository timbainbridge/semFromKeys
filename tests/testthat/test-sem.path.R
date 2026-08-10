test_that(
  "'sem.path' works with cfa_fit, extra, and items with 'orth_items = FALSE'",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
    extra <- "grit_c ~~ hope_a + hope_p"
    items <- paste0("bfi_", c("c", "n"), "1_1")
    sem_fit <- sem.path(
      path, BFIGritHope, cfa_fit, items, extra = extra, orth_items = FALSE
    )
    expect_equal(length(sem_fit), 6)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 5)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 7)
  }
)
test_that(
  "'sem.path' works with cfa_fit and items",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
    items <- paste0("bfi_", c("c", "n"), "1_1")
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit, items, fit_save = FALSE)
    expect_equal(length(sem_fit), 5)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 5)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 6)
  }
)
test_that(
  "item in 'path' not in items, but in 'data'",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit, fit_save = FALSE)
    expect_equal(length(sem_fit), 5)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 5)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 6)
  }
)
test_that(
  "item in 'path' not in data",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n10_1 + bfi_c1_1\nhope_p ~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit), "'bfi_n10_1' is in 'path'"
    )
  }
)
test_that(
  "y variable not a factor nor in data",
  {
    path <- "grit_p1 ~ grit_c + hope_p + bfi_n10_1 + bfi_c1_1\nhope_p ~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit), "'grit_p1' is in 'path'"
    )
  }
)
test_that(
  "x variable not a factor nor in data",
  {
    path <- "grit_p ~ grit_c1 + hope_p + bfi_n10_1 + bfi_c1_1\nhope_p ~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit), "'grit_c1' is in 'path'"
    )
  }
)
test_that(
  "'extra' variable not a factor nor in data",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
    items <- paste0("bfi_", c("c", "n"), "1_1")
    extra <- "grit_p1 ~~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit, items, extra = extra),
      "'grit_p1' is in 'extra'"
    )
  }
)
test_that(
  "Structural item also a measurement indicator",
  {
    path <- "grit_p ~ grit_c + hope_p + hope_a_1 + bfi_c1_1\nhope_p ~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit),
      "'hope_a_1' is a structural variable"
    )
  }
)
