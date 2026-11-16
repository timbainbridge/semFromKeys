test_that(
  "Works with keys, keys_g, and items",
  {
    r2d <- r2.change(
      BFIGritHope, X = "grit_c", Y = "grit_p", Z = c("hope", "bfi_c2_2"),
      keys = keys, keys_g = keys_g, keys_b = keys_b, fit_save = FALSE
    )
    expect_equal(length(r2d), 4)
    expect_equal(sum(sapply(r2d$fit, function(x) class(x) != "lavaan")), 0)
    expect_equal(length(r2d$structural), 3)
    expect_all_equal(
      c(
        length(r2d$fit), length(r2d$structural$b), length(r2d$structural$r),
        length(r2d$dr2)
      ),
      2
    )
    expect_equal(
      nrow(r2d$structural$b$full), nrow(r2d$structural$b$reduced) + 1
    )
  }
)
test_that(
  "Works with keys, keys_g, items, and 'fit_save = TRUE'",
  {
    r2d <- r2.change(
      BFIGritHope, X = "grit_c", Y = "grit_p", Z = c("hope", "bfi_c2_2"),
      keys = keys, keys_g = keys_g, keys_b = keys_b, fit_save = TRUE
    )
    expect_equal(length(r2d), 5)
    expect_equal(nrow(r2d$fit_measures), 2)
  }
)
test_that(
  "Bootstrap CIs work with check = TRUE, save_out = TRUE",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    name <- "dr2"
    check_save1 <- suppressWarnings(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "hope",
        keys = keys, keys_g = keys_g, keys_b = keys_b, miss = "default",
        boot = 20, boot_ci = .5, seed = 1, fit_save = FALSE,
        name = name, check = FALSE, save_out = TRUE
      )
    )
    expect_equal(length(check_save1), 4)
    expect_equal(length(check_save1$dr2), 5)
    expect_all_true(
      c(
        dir.exists(file.path(cache_dir, paste0(name, "_c"))),
        dir.exists(file.path(cache_dir, paste0(name, "_b"))),
        file.exists(
          file.path(
            cache_dir, paste0(name, "_ci"), paste0(name, "_ci_params.rds")
          )
        ),
        file.exists(
          file.path(cache_dir, paste0(name, "_ci"), paste0(name, "_ci_dr2.rds"))
        )
      )
    )
    check_save2 <- expect_no_message(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "hope",
        keys = keys, keys_g = keys_g, keys_b = keys_b, miss = "default",
        boot = 20, boot_ci = .5, seed = 1, fit_save = FALSE,
        name = name, check = TRUE, save_out = TRUE
      ),
      message = "\\d / \\d"
    )
    expect_identical(check_save1, check_save2)
    cache.clean(0, interactive = FALSE)
  }
)
test_that(
  "Works with an item as X",
  {
    r2d <- r2.change(
      BFIGritHope, Z = "grit_c", Y = "grit_p", X = "bfi_c2_2",
      keys = keys, fit_save = FALSE
    )
    expect_equal(length(r2d), 4)
    expect_equal(length(r2d$dr2), 2)
  }
)
test_that(
  "Works with an item as Y",
  {
    r2d <- r2.change(
      BFIGritHope, X = "grit_c", Z = "grit_p", Y = "bfi_c2_2",
      keys = keys, fit_save = FALSE
    )
    expect_equal(length(r2d), 4)
    expect_equal(length(r2d$dr2), 2)
  }
)
test_that(
  "Works with extra",
  {
    r2d <- r2.change(
      BFIGritHope, X = "grit_c", Y = "grit_p", Z = "hope",
      extra = c("grit_c_1 ~~ grit_p_1", ""),
      keys = keys, keys_g = keys_g, keys_b = keys_b, fit_save = TRUE
    )
    expect_equal(length(r2d), 5)
    expect_equal(nrow(r2d$fit_measures), 2)
  }
)
test_that(
  "X, Y, Z not in keys, data",
  {
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c0", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE
      ),
      "'X' is not included in 'keys', 'keys_g', or 'data'."
    )
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p0", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE
      ),
      "'Y' is not included in 'keys', 'keys_g', or 'data'."
    )
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_20",
        keys = keys, fit_save = FALSE
      ),
      "'Z' variable is not included in 'keys', 'keys_g', or 'data'."
    )
  }
)
test_that(
  "keys_g or keys_b not specified",
  {
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "hope",
        keys = keys, keys_g = keys_g, fit_save = FALSE
      ),
      "'keys_g' is specified and 'keys_b' is not"
    )
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, keys_b = keys_b, fit_save = FALSE
      ),
      "'keys_b' is specified and 'keys_g' is not"
    )
  }
)
test_that(
  "Same name for keys, keys_g",
  {
    keys2 <- keys
    names(keys2)[3] <- "hope"
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "hope",
        keys = keys2, keys_g = keys_g, keys_b = keys_b, fit_save = FALSE
      ),
      "'keys' has the same name as an element of 'keys_g'"
    )
  }
)
test_that(
  "Invalid 'boot_ci' values",
  {
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE, boot = 1000, boot_ci = 1
      ),
      "'boot_ci' must be greater than 0 and less than 1."
    )
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE, boot = 1000, boot_ci = 0
      ),
      "'boot_ci' must be greater than 0 and less than 1."
    )
  }
)
test_that(
  "Warnings for boot",
  {
    expect_warning(
      r2.change(
        BFIGritHope[1:199,], X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE, boot = 20
      ),
      "'data' has < 200 rows."
    )
    expect_warning(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE, boot = 20
      ),
      "extreme order statistics used as endpoints"
    )
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE, boot = 1
      ),
      "The number of bootstraps is extremely small"
    )
  }
)
test_that(
  "Items in keys",
  {
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "grit_c_1",
        keys = keys, fit_save = FALSE
      ),
      "one element of 'items' is in 'keys'"
    )
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "hope_a_1", Z = "hope",
        keys = keys, keys_g = keys_g, keys_b = keys_b, fit_save = FALSE
      ),
      "one element of 'items' is in 'keys_g'"
    )
  }
)
test_that(
  "Items in extra but not keys",
  {
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE, extra = "grit_p_1 ~~ bfi_c2_1"
      ),
      "was found in 'extra' but does not match"
    )
    expect_error(
      r2.change(
        BFIGritHope, X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE, extra = "bfi_c2_3 ~~ bfi_c2_1"
      ),
      "items were found in 'extra' but do not match"
    )
  }
)
test_that(
  "Base fails to converge",
  {
    expect_warning(
      r2.change(
        BFIGritHope[1:10, ], X = "grit_c", Y = "grit_p", Z = "bfi_c2_2",
        keys = keys, fit_save = FALSE
      ),
      "A sensible solution to the baseline model was not found."
    )
  }
)
