# Load data
d <- BFIGritHope

# Create keys
# CFAs
cfa_keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
cfa_keys <- sapply(cfa_keys0, function(x) names(d)[grep(x, names(d))])

# EFAs
# Using only 3 factors to save time on checks
efa_keys0 <- paste0("bfi_", c("e", "a", "c"))#, "n", "o"))

# Using less than all items to save time on checks
efa_keys <- sapply(
  efa_keys0,
  function(x) names(d)[grep(paste0(x, "\\d_[1-2]"), names(d))],
  simplify = FALSE
)

# Bifactors
bif_keys_g0 <- c("grit", "hope")
bif_keys_g <- sapply(bif_keys_g0, function(x) names(d)[grep(x, names(d))])
bif_keys_b <- sapply(
  bif_keys_g0, function(x) cfa_keys0[grep(x, cfa_keys0)], simplify = FALSE
)
# Fix negative variances in hope
bif_keys_b$hope <- bif_keys_b$hope[-1]

# Create fit objects to include
fit_save <- FALSE
cfa_fit <- cfa.from.keys(cfa_keys, d, check = FALSE, fit_save = fit_save)$fit
efa_fit <-
  efa.from.keys(efa_keys, d, check = FALSE, fit_save = fit_save)$fit$efa
bif_fit <- bifactor.from.keys(
  bif_keys_g, bif_keys_b, cfa_keys, d, check = FALSE, fit_save = fit_save
)$fit

# Test normal behaviour
# EFA and CFA only
esem_fit <- esem.from.mods(
  efa_fit, cfa_fit, bif_fit = NULL, d = d, fit_save = fit_save, check = FALSE
)
expect_equal(length(esem_fit), 4)
expect_equal(length(esem_fit$fit), length(cfa_keys))
expect_equal(length(esem_fit$par), length(cfa_keys))
expect_equal(sum(sapply(esem_fit$fit, function(x) class(x) != "lavaan")), 0)
expect_equal(length(esem_fit$b), length(cfa_keys))
expect_equal(nrow(esem_fit$r2), length(cfa_keys))

# EFA and bifactor only
esem_fit <- esem.from.mods(
  efa_fit, bif_fit = bif_fit, d = d, fit_save = fit_save, check = FALSE
)
expect_equal(length(esem_fit), 4)
expect_equal(length(esem_fit$fit), length(bif_keys_g))
expect_equal(length(esem_fit$par), length(bif_keys_g))
expect_equal(sum(sapply(esem_fit$fit, function(x) class(x) != "lavaan")), 0)
expect_equal(length(esem_fit$b), length(bif_keys_g))
expect_equal(nrow(esem_fit$r2), length(bif_keys_g))

# EFA and both CFA and bifactor
esem_fit <- esem.from.mods(
  efa_fit, cfa_fit = cfa_fit, bif_fit = bif_fit, d = d,
  fit_save = fit_save, check = FALSE
)
expect_equal(length(esem_fit), 4)
expect_equal(length(esem_fit$fit), length(bif_keys_g) + length(cfa_keys))
expect_equal(length(esem_fit$par), length(bif_keys_g) + length(cfa_keys))
expect_equal(sum(sapply(esem_fit$fit, function(x) class(x) != "lavaan")), 0)
expect_equal(length(esem_fit$b), length(bif_keys_g) + length(cfa_keys))
expect_equal(nrow(esem_fit$r2), length(bif_keys_g) + length(cfa_keys))

# Neither cfa_fit nor bif_fit set
expect_error(
  esem.from.mods(efa_fit, d = d, fit_save = fit_save, check = FALSE),
  "one of `cfa_fit` and `bif_fit` must be specified"
)

# Object of type lavaan
expect_error(
  esem.from.mods(
    efa_fit, cfa_fit = list(a = 1:2, b = 3:4), d = d,
    fit_save = fit_save, check = FALSE
  ),
  "`cfa_fit` are not objects of type lavaan"
)
expect_error(
  esem.from.mods(
    efa_fit, bif_fit = list(a = 1:2, b = 3:4), d = d,
    fit_save = fit_save, check = FALSE
  ),
  "`bif_fit` are not objects of type lavaan"
)
expect_error(
  esem.from.mods(
    efa_fit = list(a = 1:2, b = 3:4), cfa_fit = cfa_fit, d = d,
    fit_save = fit_save, check = FALSE
  ),
  "`efa_fit` is not an object of type lavaan"
)

# Different names to factor names
expect_warning(
  esem.from.mods(
    efa_fit, setNames(cfa_fit, nm = letters[1:4]), d = d,
    fit_save = fit_save, check = FALSE
  ),
  "names of `cfa_fit` do not match the factor names"
)
expect_warning(
  esem.from.mods(
    efa_fit, bif_fit = setNames(bif_fit, nm = letters[1:2]), d = d,
    fit_save = fit_save, check = FALSE
  ),
  "names of `bif_fit` do not match the general factor names"
)

# efa_fit is NULL
expect_error(
  esem.from.mods(
    efa_fit = NULL, cfa_fit, d = d, fit_save = fit_save, check = FALSE
  ),
  "`efa_fit` is NULL"
)

# Multi-factor CFA
cfa2_mod <- paste0(
  mapply(
    x = cfa_keys[1:2], y = names(cfa_keys[1:2]),
    FUN = function(x, y) {
      paste(y, "=~", paste(x, collapse = " + "))
    }
  ),
  collapse = "\n"
)
cfa2_fit <- list(grit = cfa(cfa2_mod, d))
expect_error(
  esem.from.mods(efa_fit, cfa2_fit, d = d, fit_save = fit_save, check = FALSE),
  "CFA containing more than one latent variable has been found"
)

# Same named factors
expect_error(
  esem.from.mods(
    efa_fit, cfa_fit[c(1, 1:4)], d = d, fit_save = fit_save, check = FALSE
  ),
  "two different models in `cfa_fit` have factors with the same name"
)
expect_error(
  esem.from.mods(
    efa_fit, bif_fit = bif_fit[c(1, 1:2)], d = d,
    fit_save = fit_save, check = FALSE
  ),
  "two different models in `bif_fit` have general factors with the same name"
)

# CFA factor with the same name as a bifactor general factor
cfa_keys2 <- cfa_keys
names(cfa_keys2)[1] <- "grit"
cfa_fit2 <- cfa.from.keys(cfa_keys2, d, fit_save = fit_save, check = FALSE)$fit
expect_error(
  esem.from.mods(efa_fit, cfa_fit2, bif_fit, d, check = FALSE),
  "models in `cfa_fit` have identically named factor"
)

# Item in a group factor but not in the general factor
# Should be sorted during bifactor creation, not here.
# No message or warning required.
bif_keys_g2 <- bif_keys_g
bif_keys_g2$grit <- bif_keys_g2$grit[-1]
bif_fit2 <- bifactor.from.keys(
  bif_keys_g2, bif_keys_b, cfa_keys, d, check = FALSE, fit_save = fit_save
)$fit
expect_no_error(
  esem.from.mods(efa_fit, bif_fit = bif_fit2, d = d, check = FALSE)
)
