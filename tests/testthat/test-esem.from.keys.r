test_that(
  "Test normal behaviour with 'fit_save = TRUE'",
  {
    esam_fit <- suppressWarnings(esem.from.keys(
      BFIGritHope, keys_e, keys[1:2], fit_save = TRUE,
      fit_measures = c("cfi", "rmsea", "chisq", "df", "pvalue")
    ))
    expect_equal(length(esam_fit), 5)
    expect_equal(length(esam_fit$fit), length(keys[1:2]))
    expect_equal(length(esam_fit$par), length(keys[1:2]))
    expect_equal(
      sum(sapply(esam_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esam_fit$b), length(keys[1:2]))
    expect_equal(nrow(esam_fit$r2), length(keys[1:2]))
  }
)
test_that(
  "Test normal behaviour with 'fit_save = FALSE'",
  {
    esam_fit <- esem.from.keys(BFIGritHope, keys_e, keys[1:2], fit_save = FALSE)
    expect_equal(length(esam_fit), 4)
    expect_equal(length(esam_fit$fit), length(keys[1:2]))
    expect_equal(length(esam_fit$par), length(keys[1:2]))
    expect_equal(
      sum(sapply(esam_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esam_fit$b), length(keys[1:2]))
    expect_equal(nrow(esam_fit$r2), length(keys[1:2]))
  }
)
test_that(
  "Test normal behaviour with alternative estimator",
  {
    esam_fit <- esem.from.keys(
      BFIGritHope, keys_e, keys[1:2], fit_save = FALSE, est = "ULS"
    )
    expect_equal(length(esam_fit), 4)
    expect_equal(length(esam_fit$fit), length(keys[1:2]))
    expect_equal(length(esam_fit$par), length(keys[1:2]))
    expect_equal(
      sum(sapply(esam_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esam_fit$b), length(keys[1:2]))
    expect_equal(nrow(esam_fit$r2), length(keys[1:2]))
  }
)
test_that(
  "'keys_e' has an empty name",
  {
    keys_e <- keys_e
    names(keys_e)[1] <- ""
    suppressWarnings(expect_error(
      esem.from.keys(BFIGritHope, keys_e, keys),
      "At least one element of 'keys_e' has an empty name"
    ))
  }
)
test_that(
  "'keys' has an empty name",
  {
    keys <- keys
    names(keys)[1] <- ""
    suppressWarnings(expect_error(
      esem.from.keys(BFIGritHope, keys_e, keys),
      "At least one element of 'keys' has an empty name"
    ))
  }
)
test_that(
  "Item not in data",
  {
    keys <- keys
    keys[[1]][1] <- "Hello"
    suppressWarnings(expect_error(
      esem.from.keys(BFIGritHope, keys_e, keys),
      "items are in a key but they are not in 'data'"
    ))
  }
)
test_that(
  "Non-list keys",
  {
    expect_warning(
      esem.from.keys(BFIGritHope, keys_e, keys$grit_c),
      "'keys' appears to be a vector of items"
    )
  }
)
test_that(
  "Test 'extra' correlation between 2 CFA items",
  {
    esam_fit <- esem.from.keys(
      BFIGritHope, keys_e, keys[1:2],
      extra = c("grit_c_1 ~~ grit_c_2", "grit_p_1 ~~ grit_p_2"),
      fit_save = FALSE
    )
    expect_equal(length(esam_fit), 4)
    expect_equal(length(esam_fit$fit), length(keys[1:2]))
    expect_equal(length(esam_fit$par), length(keys[1:2]))
    expect_equal(
      sum(sapply(esam_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esam_fit$b), length(keys[1:2]))
    expect_equal(nrow(esam_fit$r2), length(keys[1:2]))
  }
)
test_that(
  "Test 'extra' correlation between 2 EFA items",
  {
    esam_fit <- esem.from.keys(
      BFIGritHope, keys_e, keys[1:2],
      extra = c("bfi_e1_1 ~~ bfi_c1_1"),
      fit_save = FALSE
    )
    expect_equal(length(esam_fit), 4)
    expect_equal(length(esam_fit$fit), length(keys[1:2]))
    expect_equal(length(esam_fit$par), length(keys[1:2]))
    expect_equal(
      sum(sapply(esam_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esam_fit$b), length(keys[1:2]))
    expect_equal(nrow(esam_fit$r2), length(keys[1:2]))
  }
)
test_that(
  "Test 'extra' correlation between CFA and EFA items",
  {
    esam_fit <- esem.from.keys(
      BFIGritHope, keys_e, keys[1:2],
      extra = c("grit_c_1 ~~ bfi_c1_1", "grit_p_1 ~~ bfi_c1_2"),
      fit_save = FALSE
    )
    expect_equal(length(esam_fit), 4)
    expect_equal(length(esam_fit$fit), length(keys[1:2]))
    expect_equal(length(esam_fit$par), length(keys[1:2]))
    expect_equal(
      sum(sapply(esam_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esam_fit$b), length(keys[1:2]))
    expect_equal(nrow(esam_fit$r2), length(keys[1:2]))
  }
)
test_that(
  "Test 'extra' correlation between CFA items and EFA factors",
  {
    expect_error(
      esem.from.keys(
        BFIGritHope, keys_e, keys[1:2], fit_save = FALSE,
        extra = c("grit_c_1 ~~ bfi_c", "grit_p_1 ~~ bfi_c")
      ),
      "in 'extra' but does not match"
    )
  }
)
test_that(
  "Test 'extra' correlation between EFA items and CFA factors",
  {
    expect_error(
      esem.from.keys(
        BFIGritHope, keys_e, keys[1:2], fit_save = FALSE,
        extra = c("grit_c ~~ bfi_c1_1", "grit_p ~~ bfi_c1_2")
      ),
      "The following items were found in 'extra'"
    )
  }
)
test_that(
  "Using extra for adding items to a latent variable (not supported, use keys)",
  {
    expect_error(
      esem.from.keys(
        BFIGritHope, keys_e, keys[1:2], fit_save = FALSE,
        extra = c("grit_c =~ bfi_c1_1")
      ),
      "was found in 'extra' but does not match"
    )
  }
)
test_that(
  "Fixing correlations",
  {
    esam_fit <- esem.from.keys(
      BFIGritHope, keys_e, keys[1:2], fit_save = FALSE,
      extra = c("grit_c_1 ~~ .2 * grit_c_2")
    )
    pe <- parameterEstimates(esam_fit$fit$grit_c, remove_step1 = FALSE)
    expect_equal(pe$est[pe$lhs == "grit_c_1" & pe$rhs == "grit_c_2"], .2)
  }
)
