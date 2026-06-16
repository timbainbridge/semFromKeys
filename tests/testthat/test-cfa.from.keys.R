test_that(
  "Test normal behaviour with fit_save = FALSE",
  {
    cfa_fit <- cfa.from.keys(keys, BFIGritHope, check = FALSE, fit_save = FALSE)
    expect_equal(length(cfa_fit), 2)
    expect_equal(length(cfa_fit$fit), length(keys))
    expect_equal(length(cfa_fit$par), length(keys))
    expect_equal(sum(sapply(cfa_fit$fit, function(x) class(x) != "lavaan")), 0)
  }
)
test_that(
  "Test normal behaviour with fit_save = TRUE",
  {
    cfa_fit <- cfa.from.keys(keys, BFIGritHope, check = FALSE, fit_save = TRUE)
    expect_equal(length(cfa_fit), 3)
    expect_equal(length(cfa_fit$fit), length(keys))
    expect_equal(length(cfa_fit$par), length(keys))
    expect_equal(sum(sapply(cfa_fit$fit, function(x) class(x) != "lavaan")), 0)
  }
)
test_that(
  "Test keys not specified",
  {
    expect_error(
      cfa.from.keys(keys = NULL, BFIGritHope, check = FALSE, fit_save = FALSE),
      "one of `kl_s` or `kl_e`"
    )
    expect_error(
      cfa.from.keys(d = BFIGritHope, check = FALSE, fit_save = FALSE),
      'argument "keys" is missing'
    )
  }
)
test_that(
  "No data provided",
  {
    expect_error(
      cfa.from.keys(keys, check = FALSE, fit_save = FALSE),
      'argument "d" is missing'
    )
  }
)
test_that(
  "Incorrect fit measures",
  {
    expect_warning(
      cfa.from.keys(
        keys, BFIGritHope, check = FALSE, fit_save = TRUE,
        fit_measures = c("NotAFitMeasure", "AlsoNotAFitMeasure")
      ),
      "All the fit measures"
    )
    expect_warning(
      cfa.from.keys(
        keys, BFIGritHope, check = FALSE, fit_save = TRUE,
        fit_measures = c("NotAFitMeasure", "AlsoNotAFitMeasure", "cfi")
      ),
      "NotA|AlsoN"
    )
  }
)
test_that(
  "Mistakes in keys",
  {
    keys_mistake <- keys
    keys_mistake$grit_c[1] <- "mistake"
    expect_error(
      cfa.from.keys(keys_mistake, BFIGritHope, check = FALSE, fit_save = FALSE),
      "items are in `kl_s` but they are not in `dat`"
    )
  }
)
test_that(
  "Non-list keys list",
  {
    expect_error(
      cfa.from.keys(keys$grit_c, BFIGritHope, check = FALSE, fit_save = FALSE),
      "`kl_s` is not a list"
    )
  }
)
test_that(
  "Short keys",
  {
    keys_l2 <- keys
    keys_l2$grit_c <- keys$grit_c[1:2]
    expect_warning(
      cfa.from.keys(keys_l2, BFIGritHope, check = FALSE, fit_save = FALSE),
      "only length 2"
    )
    keys_l1 <- keys
    keys_l1$grit_c <- keys$grit_c[1]
    expect_error(
      cfa.from.keys(keys_l1, BFIGritHope, check = FALSE, fit_save = FALSE),
      "only length 1"
    )
  }
)
test_that(
  "Unrecognised missing specification",
  {
    expect_error(
      cfa.from.keys(
        keys, BFIGritHope, check = FALSE, fit_save = FALSE, miss = "banana"
      ),
      "invalid value in missing"
    )
  }
)
test_that(
  "Inputs incorrectly not strings",
  {
    expect_error(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = FALSE,
        name = 42
      ),
      "`name` is not a character"
    )
    expect_error(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = FALSE,
        name = NULL
      ),
      "`name` is not a character"
    )
    expect_error(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = FALSE,
        out_dir = 42
      ),
      "`out_dir` is not a character"
    )
  }
)
test_that(
  "Inputs not strings when it doesn't matter",
  {
    expect_no_error(
      cfa.from.keys(
        keys, BFIGritHope, check = FALSE, save_out = FALSE, fit_save = FALSE,
        name = 42
      )
    )
    expect_no_error(
      cfa.from.keys(
        keys, BFIGritHope, check = FALSE, save_out = FALSE, fit_save = FALSE,
        name = NULL
      )
    )
    expect_no_error(
      cfa.from.keys(
        keys, BFIGritHope, check = FALSE, save_out = FALSE, fit_save = FALSE,
        out_dir = 42
      )
    )
  }
)
test_that(
  "Data incorrectly specified",
  {
    expect_error(
      cfa.from.keys(keys, BFIGritHope[NULL, ], check = FALSE, fit_save = FALSE),
      "some variables have no values"
    )
    expect_error(
      cfa.from.keys(keys, d = "cfa", check = FALSE, fit_save = FALSE),
      "items are in `kl_s` but they are not in `dat`"
    )
  }
)
test_that(
  "Test `save_out = TRUE` file creation and `check = TRUE` correctly loading",
  {
    out_dir <- withr::local_tempdir(tmpdir = "tests/testthat")
    name <- "cfa"
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      name = name, out_dir = out_dir
    )
    expect_all_true(
      c(
        file.exists(file.path(out_dir, name, paste0(name, "_fit.rds"))),
        file.exists(file.path(out_dir, name, paste0(name, "_par.rds"))),
        file.exists(file.path(out_dir, name, paste0(name, "_fit_m.rds"))),
        file.exists(file.path(out_dir, name, paste0(name, "_mod.rds"))),
        file.exists(file.path(out_dir, name, paste0(name, "_hash.rds")))
      )
    )
    expect_no_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name, out_dir = out_dir
      ),
      message = "\\d / \\d"
    )
    check_fit2 <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
      name = name, out_dir = out_dir
    )
    expect_identical(check_fit, check_fit2)
  }
)
test_that(
  "Test partial running on `check = TRUE` after changes to a model",
  {
    out_dir <- withr::local_tempdir(tmpdir = "tests/testthat")
    name <- "cfa"
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      name = name, out_dir = out_dir
    )
    # Change a model
    cfa_mod <- readRDS(file.path(out_dir, name, paste0(name, "_mod.rds")))
    cfa_mod[3] <- sub(" \\+ hope_a_4", "", cfa_mod[3])
    saveRDS(cfa_mod, file.path(out_dir, name, paste0(name, "_mod.rds")))
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name, out_dir = out_dir
      ),
      "3 / \\d"
    )
    saveRDS(cfa_mod, file.path(out_dir, name, paste0(name, "_mod.rds")))
    expect_no_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name, out_dir = out_dir
      ),
      message = "([1-2]|4) / \\d"
    )
  }
)
test_that(
  "Test partial running on `check = TRUE` after adding fit measures",
  {
    out_dir <- withr::local_tempdir(tmpdir = "tests/testthat")
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      out_dir = out_dir, fit_measures = "cfi"
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
        out_dir = out_dir, fit_measures = c("cfi", "rmsea")
      ),
      "1 / \\d"
    )
  }
)
test_that(
  "Test partial running on `check = TRUE` after changes to a data hash",
  {
    out_dir <- withr::local_tempdir(tmpdir = "tests/testthat")
    name <- "cfa"
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      name = name, out_dir = out_dir
    )
    # Change a model
    cfa_hash <- readRDS(file.path(out_dir, name, paste0(name, "_hash.rds")))
    cfa_hash[3] <- "helloworld123"
    saveRDS(cfa_hash, file.path(out_dir, name, paste0(name, "_hash.rds")))
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name, out_dir = out_dir
      ),
      "3 / \\d"
    )
    saveRDS(cfa_hash, file.path(out_dir, name, paste0(name, "_hash.rds")))
    expect_no_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name, out_dir = out_dir
      ),
      message = "([1-2]|4) / \\d"
    )
  }
)
test_that(
  "Test running on `check = TRUE` after changes to miss",
  {
    out_dir <- withr::local_tempdir(tmpdir = "tests/testthat")
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      out_dir = out_dir
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        out_dir = out_dir, miss = "pairwise"
      ),
      "1 / \\d"
    )
  }
)
test_that(
  "Test running on `check = TRUE` after changes to est",
  {
    out_dir <- withr::local_tempdir(tmpdir = "tests/testthat")
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      out_dir = out_dir
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        out_dir = out_dir, est = "MLR"
      ),
      "1 / \\d"
    )
  }
)
test_that(
  "Test running on `check = TRUE` after changes to std.lv",
  {
    out_dir <- withr::local_tempdir(tmpdir = "tests/testthat")
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      out_dir = out_dir
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        out_dir = out_dir, std.lv = FALSE
      ),
      "1 / \\d"
    )
  }
)
