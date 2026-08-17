test_that(
  "Test normal behaviour with 'save_out = TRUE'",
  {
    esam_fit <- suppressWarnings(esem.from.keys(
      BFIGritHope, keys_e, keys[1:2],
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
  "Test normal behaviour with 'save_out = FALSE'",
  {
    esam_fit <- suppressWarnings(esem.from.keys(
      BFIGritHope, keys_e, keys[1:2], fit_save = FALSE
    ))
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
    expect_error(
      esem.from.keys(BFIGritHope, keys_e, keys$grit_c),
      "'keys' is not a list"
    )
  }
)
