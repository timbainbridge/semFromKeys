test_that(
  "'sem.path' works with cfa_fit, extra, and items",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
    extra <- "grit_c ~~ hope_a + hope_p"
    items <- paste0("bfi_", c("c", "n"), "1_1")
    sem_fit <- sem.path(
      path, BFIGritHope, cfa_fit, items, extra = extra, fit_save = TRUE
    )
    expect_equal(length(sem_fit), 4)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_equal(length(sem_fit$b), 2)
    expect_equal(nrow(sem_fit$r2), length(keys))
  }
)
test_that(
  "'sem.path' works with cfa_fit, items, and 'orth_items = TRUE'",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
    items <- paste0("bfi_", c("c", "n"), "1_1")
    sem_fit <- sem.path(
      path, BFIGritHope, cfa_fit, items, extra = extra, orth_items = TRUE
    )
    expect_equal(length(sem_fit), 4)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_equal(length(sem_fit$b), 2)
    expect_equal(nrow(sem_fit$r2), length(keys))
  }
)
test_that(
  "item in 'path' not in items, but in 'data'",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
    sem_fit <- sem.path(
      path, BFIGritHope, cfa_fit, extra = extra, orth_items = TRUE
    )
    expect_equal(length(sem_fit), 4)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_equal(length(sem_fit$b), 2)
    expect_equal(nrow(sem_fit$r2), length(keys))
  }
)
test_that(
  "item in 'path' not in data",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n10_1 + bfi_c1_1\nhope_p ~ hope_a"
    expect_error(
      sem.path(
        path, BFIGritHope, cfa_fit, extra = extra, orth_items = TRUE
      ),
      "A depenent variable in 'path' is not in 'items'"
    )
  }
)

# Check y_miss works
# Check item_miss works
# Check extra_miss works
# Check item_in_cfa
