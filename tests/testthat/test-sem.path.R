test_that(
  "'sem.path' works with correlation between x var and y residual",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_c1_1\nhope_p ~ hope_a"
    extra <- "grit_c ~~ hope_p"
    sem_fit <-
      sem.path(path, BFIGritHope, cfa_fit, extra = extra, fit_save = TRUE)
    expect_equal(length(sem_fit), 6)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 4)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 4)
  }
)
test_that(
  "'sem.path' works with a fixed 0 correlation between x vars",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_c1_1\nhope_p ~ hope_a"
    extra <- "grit_c ~~ 0 * hope_a"
    sem_fit <-
      sem.path(path, BFIGritHope, cfa_fit, extra = extra, fit_save = FALSE)
    expect_equal(length(sem_fit), 5)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 4)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 3)
    expect_equal(sem_fit$cors$est.std[sem_fit$cors$rhs == "hope_a"], 0)
  }
)
test_that(
  "'sem.path' works with fixed non-zero path",
  {
    path <- "grit_p ~ .5 * grit_c + .5 * hope_p + bfi_c1_1\nhope_p ~ hope_a"
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit, fit_save = FALSE)
    expect_equal(length(sem_fit), 5)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 4)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 3)
    # No check for fixed values as these are unstandardised.
    # The test is primarily to check that '.5's are correctly removed from
    # x_vars and y_vars.
  }
)
test_that(
  "'sem.path' with a specified estimator (few work)",
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
test_that(
  "Variable in 'path' not in data or a latent variable",
  {
    path <- "grit_p ~ grit_c + hope_p + bfi_n1_10 + bfi_c1_1\nhope_p ~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit), "'bfi_n1_10' is in 'path'"
    )
    path <- "grit_p1 ~ grit_c + hope_p + bfi_n10_1 + bfi_c1_1\nhope_p ~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit), "'grit_p1' is in 'path'"
    )
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
    extra <- "grit_p1 ~~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, cfa_fit, extra = extra),
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
test_that(
  "'sem.path' with no correlations",
  {
    path <- "grit_p ~ hope_p\nhope_p ~ hope_a"
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit[2:4])
    expect_equal(length(sem_fit), 5)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 2)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_true(!"cors" %in% names(sem_fit))
  }
)
test_that(
  "'sem.path' with no correlations",
  {
    path <- "grit_p ~ hope_p\nhope_p ~ hope_a"
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit[2:4], fit_save = FALSE)
    expect_equal(length(sem_fit), 4)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 2)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_true(!"cors" %in% names(sem_fit))
  }
)
test_that(
  "Multi-line extra with correlated item-residuals",
  {
    path <- "grit_p ~ grit_c + hope_p\nhope_p ~ hope_a"
    extra <- "grit_c ~~ hope_a\ngrit_c_1 ~~ grit_p_1"
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit, extra = extra)
    expect_equal(length(sem_fit), 6)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 3)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 2)
  }
)
test_that(
  "Not objects of type 'lavaan'",
  {
    path <- "grit_p ~ grit_c + hope_p\nhope_p ~ hope_a"
    fit <- cfa_fit
    fit[2] <- "Hello"
    expect_error(
      sem.path(path, BFIGritHope, fit),
      "not objects of type lavaan"
    )
  }
)
test_that(
  "Same name CFAs",
  {
    path <- "grit_p ~ grit_c + hope_p\nhope_p ~ hope_a"
    fit <- cfa_fit
    fit[2] <- cfa_fit[1]
    expect_error(
      sem.path(path, BFIGritHope, fit),
      "two different models in 'cfa_fit' have factors with the same name."
    )
  }
)
test_that(
  "Model with more than 1 factor",
  {
    path <- "grit_p ~ grit_c + hope_p\nhope_p ~ hope_a"
    expect_error(
      sem.path(path, BFIGritHope, bif_fit),
      "A CFA containing more than one latent variable has been found"
    )
  }
)
test_that(
  "Non-list single CFA as cfa_fit input",
  {
    path <- "grit_p ~ grit_c_1 + hope_p_1\nhope_p_1 ~ hope_a_1"
    sem_fit <- sem.path(path, BFIGritHope, cfa_fit[[2]], fit_save = FALSE)
    expect_equal(length(sem_fit), 5)
    expect_true(inherits(sem_fit$fit, "lavaan"))
    expect_true(inherits(sem_fit$par_std, "lavaan.data.frame"))
    expect_equal(nrow(sem_fit$b), 3)
    expect_equal(nrow(sem_fit$r2), 2)
    expect_equal(nrow(sem_fit$cors), 1)
  }
)
