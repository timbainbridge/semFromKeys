# Create keys
# CFAs
cfa_keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
cfa_keys <- sapply(
  cfa_keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
)

# EFAs
# Using only 3 factors to save time on checks
efa_keys0 <- paste0("bfi_", c("e", "a", "c"))

# Using less than all items to save time on checks
efa_keys <- sapply(
  efa_keys0,
  function(x) {
    names(BFIGritHope)[grep(paste0(x, "\\d_[1-2]"), names(BFIGritHope))]
  },
  simplify = FALSE
)

# Bifactors
bif_keys_g0 <- c("grit", "hope")
bif_keys_g <- sapply(
  bif_keys_g0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
)
bif_keys_b <- sapply(
  bif_keys_g0, function(x) cfa_keys0[grep(x, cfa_keys0)], simplify = FALSE
)
# Fix negative variances in hope
bif_keys_b$hope <- bif_keys_b$hope[-1]

# Create fit objects to include
cfa_fit <- cfa.from.keys(cfa_keys, BFIGritHope, check = FALSE, fit_save = FALSE)$fit
efa_fit <-
  efa.from.keys(efa_keys, BFIGritHope, check = FALSE, fit_save = FALSE)$fit$efa
bif_fit <- bifactor.from.keys(
  bif_keys_g, bif_keys_b, cfa_keys, BFIGritHope, check = FALSE, fit_save = FALSE
)$fit

test_that(
  "Test normal behaviour: EFA and CFA only",
  {
    esem_fit <- esem.from.mods(
      efa_fit, cfa_fit, bif_fit = NULL, d = BFIGritHope,
      fit_save = FALSE, check = FALSE
    )
    expect_equal(length(esem_fit), 4)
    expect_equal(length(esem_fit$fit), length(cfa_keys))
    expect_equal(length(esem_fit$par), length(cfa_keys))
    expect_equal(sum(sapply(esem_fit$fit, function(x) class(x) != "lavaan")), 0)
    expect_equal(length(esem_fit$b), length(cfa_keys))
    expect_equal(nrow(esem_fit$r2), length(cfa_keys))
  }
)
test_that(
  "Test normal behaviour: EFA and bifactor only",
  {
    esem_fit <- esem.from.mods(
      efa_fit, bif_fit = bif_fit, d = BFIGritHope,
      fit_save = FALSE, check = FALSE
    )
    expect_equal(length(esem_fit), 4)
    expect_equal(length(esem_fit$fit), length(bif_keys_g))
    expect_equal(length(esem_fit$par), length(bif_keys_g))
    expect_equal(sum(sapply(esem_fit$fit, function(x) class(x) != "lavaan")), 0)
    expect_equal(length(esem_fit$b), length(bif_keys_g))
    expect_equal(nrow(esem_fit$r2), length(bif_keys_g))
  }
)
test_that(
  "Test normal behaviour: EFA and both CFA and bifactor",
  {
    esem_fit <- esem.from.mods(
      efa_fit, cfa_fit = cfa_fit, bif_fit = bif_fit, d = BFIGritHope,
      fit_save = FALSE, check = FALSE
    )
    expect_equal(length(esem_fit), 4)
    expect_equal(length(esem_fit$fit), length(bif_keys_g) + length(cfa_keys))
    expect_equal(length(esem_fit$par), length(bif_keys_g) + length(cfa_keys))
    expect_equal(sum(sapply(esem_fit$fit, function(x) class(x) != "lavaan")), 0)
    expect_equal(length(esem_fit$b), length(bif_keys_g) + length(cfa_keys))
    expect_equal(nrow(esem_fit$r2), length(bif_keys_g) + length(cfa_keys))
  }
)
test_that(
  "Neither cfa_fit nor bif_fit set",
  {
    expect_error(
      esem.from.mods(efa_fit, d = BFIGritHope, fit_save = FALSE, check = FALSE),
      "one of `cfa_fit` and `bif_fit` must be specified"
    )
  }
)
test_that(
  "Non-lavaan object inputs",
  {
    expect_error(
      esem.from.mods(
        efa_fit, cfa_fit = list(a = 1:2, b = 3:4), d = BFIGritHope,
        fit_save = FALSE, check = FALSE
      ),
      "`cfa_fit` are not objects of type lavaan"
    )
    expect_error(
      esem.from.mods(
        efa_fit, bif_fit = list(a = 1:2, b = 3:4), d = BFIGritHope,
        fit_save = FALSE, check = FALSE
      ),
      "`bif_fit` are not objects of type lavaan"
    )
    expect_error(
      esem.from.mods(
        efa_fit = list(a = 1:2, b = 3:4), cfa_fit = cfa_fit, d = BFIGritHope,
        fit_save = FALSE, check = FALSE
      ),
      "`efa_fit` is not an object of type lavaan"
    )
  }
)
test_that(
  "Different names to factor names",
  {
    expect_warning(
      esem.from.mods(
        efa_fit, setNames(cfa_fit, nm = letters[1:4]), d = BFIGritHope,
        fit_save = FALSE, check = FALSE
      ),
      "names of `cfa_fit` do not match the factor names"
    )
    expect_warning(
      esem.from.mods(
        efa_fit, bif_fit = setNames(bif_fit, nm = letters[1:2]),
        d = BFIGritHope, fit_save = FALSE, check = FALSE
      ),
      "names of `bif_fit` do not match the general factor names"
    )
  }
)
test_that(
  "efa_fit is NULL",
  {
    expect_error(
      esem.from.mods(
        efa_fit = NULL, cfa_fit, d = BFIGritHope,
        fit_save = FALSE, check = FALSE
      ),
      "`efa_fit` is NULL"
    )
  }
)
test_that(
  "Multi-factor CFA",
  {
    cfa2_mod <- paste0(
      mapply(
        x = cfa_keys[1:2], y = names(cfa_keys[1:2]),
        FUN = function(x, y) paste(y, "=~", paste(x, collapse = " + "))
      ),
      collapse = "\n"
    )
    cfa2_fit <- list(grit = lavaan::cfa(cfa2_mod, BFIGritHope))
    expect_error(
      esem.from.mods(
        efa_fit, cfa2_fit, d = BFIGritHope, fit_save = FALSE, check = FALSE
      ),
      "CFA containing more than one latent variable has been found"
    )
  }
)
test_that(
  "Factors with the same names",
  {
    expect_error(
      esem.from.mods(
        efa_fit, cfa_fit[c(1, 1:4)], d = BFIGritHope,
        fit_save = FALSE, check = FALSE
      ),
      "two different models in `cfa_fit` have factors with the same name"
    )
    expect_error(
      esem.from.mods(
        efa_fit, bif_fit = bif_fit[c(1, 1:2)], d = BFIGritHope,
        fit_save = fit_save, check = FALSE
      ),
      "different models in `bif_fit` have general factors with the same name"
    )
  }
)
test_that(
  "CFA factor with the same name as a bifactor general factor",
  {
    cfa_keys2 <- cfa_keys
    names(cfa_keys2)[1] <- "grit"
    cfa_fit2 <- cfa.from.keys(
      cfa_keys2, BFIGritHope, fit_save = FALSE, check = FALSE
    )$fit
    expect_error(
      esem.from.mods(efa_fit, cfa_fit2, bif_fit, BFIGritHope, check = FALSE),
      "models in `cfa_fit` have identically named factor"
    )
  }
)
# Item in a group factor but not in the general factor should be sorted during
# bifactor creation, not here.
test_that(
  "No error from items in group factor not in general factor",
  {
    bif_keys_g2 <- bif_keys_g
    bif_keys_g2$grit <- bif_keys_g2$grit[-1]
    bif_fit2 <- suppressWarnings(  # In preamble, not important for the test.
      bifactor.from.keys(
        bif_keys_g2, bif_keys_b, cfa_keys, BFIGritHope,
        check = FALSE, fit_save = FALSE
      )$fit
    )
    expect_no_error(
      esem.from.mods(
        efa_fit, bif_fit = bif_fit2, d = BFIGritHope, check = FALSE
      )
    )
  }
)
