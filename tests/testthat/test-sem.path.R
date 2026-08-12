test_that(
  "'sem.path' works with cfa_fit, extra, and items with 'orth_x = TRUE'",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_c1_1\nhope_p ~ hope_a"
    extra <- "grit_c ~~ hope_p"
    sem_fit <-
      sem.path(path, BFIGritHope, cfa_fit, extra = extra, orth_x = TRUE)
    expect_equal(length(sem_fit), 6)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 4)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 4)
  }
)
test_that(
  "'sem.path' works with cfa_fit and items with 'orth_x = FALSE'",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_c1_1\nhope_p ~ hope_a"
    sem_fit <- sem.path(
      path, BFIGritHope, cfa_fit, fit_save = FALSE, orth_x = FALSE
    )
    expect_equal(length(sem_fit), 5)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 5)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 3)
  }
)
test_that(
  "'sem.path' with a specified estimator",
  {
    path <- "grit_p ~ grit_c + hope_p\nhope_p ~ hope_a"
    sem_fit <-
      sem.path(path, BFIGritHope, cfa_fit, est = "ULS", miss = "listwise")
    expect_equal(length(sem_fit), 6)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 3)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 1)
  }
)

# TODO: See if there's some way to get sam to include factors that are only
# correlated in the sem. It appears to treat these correlations as 0.
# TODO: Does this even matter? If interpretational confounding is not an issue,
# then adding a variable should change estimates of other structural parameters.
# Check that this is the case. If so then incremental validity will be much
# easier.
# TODO: If so, extra_vars can be removed as all vars will be x or y.

test_that(
  "Variable in 'extra' not in 'path' (typically for incremental validity)",
  {
    path <- "grit_p ~ hope_p\nhope_p ~ hope_a"
    extra <- "grit_c ~~ grit_p + hope_p + hope_a"
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit, extra = extra)
    expect_equal(length(sem_fit), 6)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 2)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 3)
    expect_false(sem_fit$cors$est.std[1] == 0)
  }
)

# TODO: Make this the standard.

# test_that(
#   "item in 'path' not in items, but in 'data'",
#   {
#     path <- "grit_p ~ grit_c + hope_p + bfi_n1_1 + bfi_c1_1\nhope_p ~ hope_a"
#     sem_fit <- sem.path(path, BFIGritHope, cfa_fit, fit_save = FALSE)
#     expect_equal(length(sem_fit), 5)
#     expect_true(inherits(sem_fit$fit, "lavaan"))
#     expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
#     expect_equal(nrow(sem_fit$b), 5)
#     expect_equal(nrow(sem_fit$r2), 2)
#     expect_equal(nrow(sem_fit$cors), 6)
#   }
# )
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

# TODO: Create tests with no correlations with both fit_save = TRUE and = FALSE.
