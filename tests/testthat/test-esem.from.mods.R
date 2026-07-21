test_that(
  "Test normal behaviour: EFA and CFA only",
  {
    esem_fit <- esem.from.mods(
      efa_fit, cfa_fit, data = BFIGritHope, fit_save = FALSE
    )
    expect_equal(length(esem_fit), 4)
    expect_equal(length(esem_fit$fit), length(keys))
    expect_equal(length(esem_fit$par), length(keys))
    expect_equal(
      sum(sapply(esem_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esem_fit$b), length(keys))
    expect_equal(nrow(esem_fit$r2), length(keys))
  }
)
test_that(
  "Test normal behaviour: EFA and bifactor only",
  {
    esem_fit <- esem.from.mods(
      efa_fit, bif_fit = bif_fit, data = BFIGritHope, fit_save = FALSE
    )
    expect_equal(length(esem_fit), 4)
    expect_equal(length(esem_fit$fit), length(keys_g))
    expect_equal(length(esem_fit$par), length(keys_g))
    expect_equal(
      sum(sapply(esem_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(length(esem_fit$b), length(keys_g))
    expect_equal(nrow(esem_fit$r2), length(keys_g))
  }
)
test_that(
  "Test normal behaviour: EFA and both CFA and bifactor with fit_save = TRUE",
  {
    esem_fit <- esem.from.mods(
      BFIGritHope, efa_fit, cfa_fit = cfa_fit, bif_fit = bif_fit,
      fit_save = TRUE
    )
    expect_equal(length(esem_fit), 5)
    expect_equal(length(esem_fit$fit), length(keys_g) + length(keys))
    expect_equal(length(esem_fit$par), length(keys_g) + length(keys))
    expect_equal(
      sum(sapply(esem_fit$fit, function(x) !inherits(x, "lavaan"))), 0
    )
    expect_equal(nrow(esem_fit$fit_measures), length(keys_g) + length(keys))
    expect_equal(length(esem_fit$b), length(keys_g) + length(keys))
    expect_equal(nrow(esem_fit$r2), length(keys_g) + length(keys))
  }
)
test_that(
  "Neither cfa_fit nor bif_fit set",
  {
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit),
      "one of 'cfa_fit' and 'bif_fit' must be specified"
    )
  }
)
test_that(
  "Non-lavaan object inputs",
  {
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit, cfa_fit = list(a = 1:2, b = 3:4)),
      "'cfa_fit' are not objects of type lavaan"
    )
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit, bif_fit = list(a = 1:2, b = 3:4)),
      "'bif_fit' are not objects of type lavaan"
    )
    expect_error(
      esem.from.mods(
        BFIGritHope, efa_fit = list(a = 1:2, b = 3:4), cfa_fit = cfa_fit
      ),
      "'efa_fit' is not an object of type lavaan"
    )
  }
)
# Currently not throwing a warning due to dynamic renaming of single models
# entered not as a list.
# test_that(
#   "Different names to factor names",
#   {
#     expect_warning(
#       esem.from.mods(
#         BFIGritHope, efa_fit, setNames(cfa_fit, nm = letters[1:4])
#       ),
#       "names of 'cfa_fit' do not match the factor names"
#     )
#     expect_warning(
#       esem.from.mods(
#         BFIGritHope, efa_fit, bif_fit = setNames(bif_fit, nm = letters[1:2])
#       ),
#       "names of 'bif_fit' do not match the general factor names"
#     )
#   }
# )
test_that(
  "efa_fit is NULL",
  {
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit = NULL, cfa_fit),
      "'efa_fit' is NULL"
    )
  }
)
test_that(
  "Multi-factor CFA",
  {
    cfa2_mod <- paste0(
      mapply(
        x = keys[1:2], y = names(keys[1:2]),
        FUN = function(x, y) paste(y, "=~", paste(x, collapse = " + "))
      ),
      collapse = "\n"
    )
    cfa2_fit <- list(grit = lavaan::cfa(cfa2_mod, BFIGritHope))
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit, cfa2_fit),
      "CFA containing more than one latent variable has been found"
    )
  }
)
test_that(
  "Factors with the same names",
  {
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit, cfa_fit[c(1, 1:4)]),
      "two different models in 'cfa_fit' have factors with the same name"
    )
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit, bif_fit = bif_fit[c(1, 1:2)]),
      "different models in 'bif_fit' have general factors with the same name"
    )
  }
)
test_that(
  "CFA factor with the same name as a bifactor general factor",
  {
    keys2 <- keys
    names(keys2)[1] <- "grit"
    cfa_fit2 <- cfa.from.keys(keys2, BFIGritHope, fit_save = FALSE)$fit
    expect_error(
      esem.from.mods(BFIGritHope, efa_fit, cfa_fit2, bif_fit),
      "models in 'cfa_fit' have identically named factor"
    )
  }
)
# Item in a group factor but not in the general factor should be sorted during
# bifactor creation, not here.
test_that(
  "No error from items in group factor not in general factor",
  {
    keys_g2 <- keys_g
    keys_g2$grit <- keys_g2$grit[-1]
    bif_fit2 <- suppressWarnings(  # In preamble, not important for the test.
      bifactor.from.keys(
        keys_g2, keys_b, keys, BFIGritHope, fit_save = FALSE
      )$fit
    )
    expect_no_error(esem.from.mods(BFIGritHope, efa_fit, bif_fit = bif_fit2))
  }
)
test_that(
  "Test 'save_out = TRUE' file creation and 'check = TRUE' correctly loading",
  {
    cache_dir <- cache.setup("tests/testthat", interactive = FALSE)
    name <- "esem"
    check_fit <- esem.from.mods(
      BFIGritHope, efa_fit, cfa_fit, check = TRUE, save_out = TRUE,
      fit_save = TRUE, name = name
    )
    expect_all_true(
      c(
        file.exists(file.path(cache_dir, name, paste0(name, "_fit.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_par_std.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_fit_m.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_mod.rds"))),
        file.exists(file.path(cache_dir, name, paste0(name, "_hash.rds")))
      )
    )
    check_fit2 <- expect_no_message(
      esem.from.mods(
        BFIGritHope, efa_fit, cfa_fit, check = TRUE, save_out = TRUE,
        fit_save = TRUE, name = name
      ),
      message = "\\d / \\d"
    )
    expect_identical(check_fit, check_fit2)
  }
)
