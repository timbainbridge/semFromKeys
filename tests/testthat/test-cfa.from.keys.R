test_that(
  "Test normal behaviour with fit_save = FALSE",
  {
    cfa_fit <- cfa.from.keys(keys, BFIGritHope, fit_save = FALSE)
    expect_equal(length(cfa_fit), 2)
    expect_equal(length(cfa_fit$fit), length(keys))
    expect_equal(length(cfa_fit$par), length(keys))
    expect_equal(sum(sapply(cfa_fit$fit, function(x) class(x) != "lavaan")), 0)
  }
)
test_that(
  "Test normal behaviour with fit_save = TRUE",
  {
    cfa_fit <- cfa.from.keys(keys, BFIGritHope, fit_save = TRUE)
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
      cfa.from.keys(keys = NULL, BFIGritHope, fit_save = FALSE),
      "one of 'keys_s' or 'keys_e'"
    )
    expect_error(
      cfa.from.keys(data = BFIGritHope, fit_save = FALSE),
      'argument "keys" is missing'
    )
  }
)
test_that(
  "No data provided",
  {
    expect_error(
      cfa.from.keys(keys, fit_save = FALSE),
      'argument "data" is missing'
    )
  }
)
test_that(
  "Various things 'not logical'",
  {
    expect_error(
      cfa.from.keys(keys, BFIGritHope, fit_save = 42),
      "'fit_save' is not logical"
    )
    expect_error(
      cfa.from.keys(keys, BFIGritHope, check = 42, fit_save = FALSE),
      "'check' is not logical"
    )
    expect_error(
      cfa.from.keys(keys, BFIGritHope, fit_save = FALSE, save_out = 42),
      "'save_out' is not logical"
    )
    expect_error(
      cfa.from.keys(keys, BFIGritHope, fit_save = FALSE, std.lv = 42),
      "'std.lv' is not logical"
    )
  }
)
test_that(
  "'fit_measures' is not a character vector",
  {
    expect_error(
      cfa.from.keys(keys, BFIGritHope, fit_save = TRUE, fit_measures = 42),
      "'fit_measures' is not a character vector"
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
      "fit measures that you specified are not recognised"
    )
  }
)
test_that(
  "Mistakes in keys",
  {
    keys_mistake <- keys
    keys_mistake$grit_c[1] <- "mistake"
    expect_error(
      cfa.from.keys(keys_mistake, BFIGritHope, fit_save = FALSE),
      "items are in 'keys_s' but they are not in 'data'"
    )
  }
)
test_that(
  "Non-list keys list",
  {
    expect_error(
      cfa.from.keys(keys$grit_c, BFIGritHope, fit_save = FALSE),
      "'keys_s' is not a list"
    )
  }
)
test_that(
  "Short keys",
  {
    keys_l2 <- keys
    keys_l2$grit_c <- keys$grit_c[1:2]
    expect_warning(
      cfa.from.keys(keys_l2, BFIGritHope, fit_save = FALSE),
      "only length 2"
    )
    keys_l1 <- keys
    keys_l1$grit_c <- keys$grit_c[1]
    expect_error(
      cfa.from.keys(keys_l1, BFIGritHope, fit_save = FALSE),
      "only length 1"
    )
  }
)
test_that(
  "Keys with the same name",
  {
    keys_nm <- keys
    names(keys_nm)[1:2] <- "grit"
    expect_error(
      cfa.from.keys(keys_nm, BFIGritHope, fit_save = FALSE),
      "two elements of 'keys_s' share the same name"
    )
  }
)
test_that(
  "Unrecognised missing specification",
  {
    expect_error(
      cfa.from.keys(keys, BFIGritHope, fit_save = FALSE, miss = "banana"),
      "invalid value in missing"
    )
  }
)
test_that(
  "Caching not set up",
  {
    expect_error(
      cfa.from.keys(keys, BFIGritHope, check = TRUE, fit_save = FALSE),
      "A cache directory is not configured"
    )
    expect_error(
      cfa.from.keys(keys, BFIGritHope, save_out = TRUE, fit_save = FALSE),
      "A cache directory is not configured"
    )
  }
)
test_that(
  "'name' not a string (but a number) when required.",
  {
    cache.setup("tests/testthat/cache", interactive = FALSE)
    expect_error(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = FALSE,
        name = 42
      ),
      "'name' is not a character"
    )
  }
)
test_that(
  "'name' not a string (but NULL) when required.",
  {
    cache.setup("tests/testthat/cache", interactive = FALSE)
    expect_error(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = FALSE,
        name = NULL
      ),
      "'name' is not a character"
    )
  }
)
test_that(
  "'name' is not a string when not required",
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
  }
)
test_that(
  "Data incorrectly specified",
  {
    expect_error(
      cfa.from.keys(keys, BFIGritHope[NULL, ], fit_save = FALSE),
      "some variables have no values"
    )
    expect_error(
      cfa.from.keys(keys, data = "cfa", fit_save = FALSE),
      "items are in 'keys_s' but they are not in 'data'"
    )
  }
)
test_that(
  "Test 'save_out = TRUE' file creation and 'check = TRUE' correctly loading",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    name <- "cfa"
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      name = name
    )
    expect_all_true(
      c(
        file.exists(file.path(cache_dir, name, paste0(name, "_fit.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_par.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_fit_m.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_mod.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_hash.rds")))
      )
    )
    check_fit2 <- expect_no_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name
      ),
      message = "\\d / \\d"
    )
    expect_identical(check_fit, check_fit2)
  }
)
test_that(
  "Cache location invalid",
  {
    expect_error(
      cache.setup(c("tests/testthat/cache", "tests/testhtat/invalid")),
      "the condition has length > 1"
    )
    expect_error(
      cache.setup(42), "'location' is not a length 1 character vector"
    )
  }
)
test_that(
  "Test partial running on 'check = TRUE' after changes to a model",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    name <- "cfa"
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      name = name
    )
    # Change a model
    cfa_mod <- readRDS(file.path(cache_dir, name, paste0(name, "_mod.rds")))
    cfa_mod[3] <- sub(" \\+ hope_a_4", "", cfa_mod[3])
    saveRDS(cfa_mod, file.path(cache_dir, name, paste0(name, "_mod.rds")))
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name
      ),
      "3 / \\d"
    )
    saveRDS(cfa_mod, file.path(cache_dir, name, paste0(name, "_mod.rds")))
    expect_no_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name
      ),
      message = "([1-2]|4) / \\d"
    )
  }
)
test_that(
  "Test running on 'check = TRUE' after changing to full 'fit_measures' set",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      fit_measures = c("chisq", "cfi", "rmsea")
    )
    check_fit2 <- expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE
      ),
      "1 / \\d"
    )
    expect_all_true(
      c(ncol(check_fit$fit_measures) == 3, ncol(check_fit2$fit_measures) >= 55)
    )
  }
)
# This is treated slightly differently to the above in sem.check
# due to distinction between 'fit_measures = "all"' and a subset.
test_that(
  "Test running on 'check = TRUE' after adding fit measures (not to full set)",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      fit_measures = "cfi"
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
        fit_measures = c("cfi", "rmsea")
      ),
      "1 / \\d"
    )
  }
)
test_that(
  "Test partial running on 'check = TRUE' after changes to a data hash",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    name <- "cfa"
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      name = name
    )
    # Change a model
    cfa_hash <- readRDS(file.path(cache_dir, name, paste0(name, "_hash.rds")))
    cfa_hash[3] <- "helloworld123"
    saveRDS(cfa_hash, file.path(cache_dir, name, paste0(name, "_hash.rds")))
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name
      ),
      "3 / \\d"
    )
    saveRDS(cfa_hash, file.path(cache_dir, name, paste0(name, "_hash.rds")))
    expect_no_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        name = name
      ),
      message = "([1-2]|4) / \\d"
    )
  }
)
test_that(
  "Test running on 'check = TRUE' after changes to miss",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        miss = "pairwise"
      ),
      "1 / \\d"
    )
  }
)
test_that(
  "Test running on 'check = TRUE' after changes to est",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        est = "MLR"
      ),
      "1 / \\d"
    )
  }
)
test_that(
  "Test running on 'check = TRUE' after changes to std.lv",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        std.lv = FALSE
      ),
      "1 / \\d"
    )
  }
)
test_that(
  "Test running on 'check = TRUE' after selecting subset of 'fit_measures'",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    check_fit <- cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE
    )
    check_fit2 <- expect_no_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE,
        fit_measures = c("chisq", "cfi", "rmsea")
      ),
      message = "\\d / \\d"
    )
    expect_all_true(
      c(ncol(check_fit$fit_measures) >= 55, ncol(check_fit2$fit_measures) == 3)
    )
  }
)
test_that(
  "Test adding a model between saved runs",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    cfa.from.keys(
      keys[-1], BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE
      ),
      "1 / \\d"
    )
    cfa.from.keys(
      keys[-4], BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE
    )
    expect_message(
      cfa.from.keys(
        keys, BFIGritHope, check = TRUE, save_out = FALSE, fit_save = TRUE
      ),
      "4 / \\d"
    )
  }
)
test_that(
  "Test deleting of cache files",
  {
    cache_dir <- cache.setup("tests/testthat/cache", interactive = FALSE)
    name <- "cfa"
    cfa.from.keys(
      keys, BFIGritHope, check = TRUE, save_out = TRUE, fit_save = TRUE,
      name = name
    )
    expect_all_true(
      c(
        file.exists(file.path(cache_dir, name, paste0(name, "_fit.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_par.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_fit_m.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_mod.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_hash.rds")))
      )
    )
    cache.clean(0, interactive = FALSE)
    expect_equal(
      length(list.files(cache_dir, full.names = TRUE, recursive = TRUE)), 0
    )
  }
)
