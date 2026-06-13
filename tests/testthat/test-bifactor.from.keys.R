# Load data
d <- BFIGritHopeImp

# Create keys
keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p", "imp_a", "imp_m", "imp_np")
keys <- sapply(keys0, function(x) names(d)[grep(x, names(d))])
keys_g0 <- c("grit", "hope", "imp")
keys_g <- sapply(keys_g0, function(x) names(d)[grep(x, names(d))])
keys_b <- sapply(keys_g0, function(x) keys0[grep(x, keys0)])

# Test normal behaviour
fit_save <- FALSE
bif_fit <- bifactor.from.keys(
  keys_g, keys_b, keys, d, check = FALSE, fit_save = fit_save
)
expect_equal(length(bif_fit), 2)
expect_equal(length(bif_fit$fit), length(keys_g))
expect_equal(length(bif_fit$par), length(keys_g))
expect_equal(sum(sapply(bif_fit$fit, function(x) class(x) != "lavaan")), 0)

fit_save <- TRUE
bif_fit <- bifactor.from.keys(
  keys_g, keys_b, keys, d, check = FALSE, fit_save = fit_save
)
expect_equal(length(bif_fit), 3)
expect_equal(length(bif_fit$fit), length(keys_g))
expect_equal(length(bif_fit$par), length(keys_g))
expect_equal(sum(sapply(bif_fit$fit, function(x) class(x) != "lavaan")), 0)

# Abnormally specified keys
# Just include CFA keys as keys
fit_save <- FALSE
expect_error(
  bifactor.from.keys(
    keys = keys_g, d = d, check = FALSE, fit_save = fit_save
  ),
  'argument "keys_g" is missing'
)

# Include CFA keys as keys_g
expect_error(
  bifactor.from.keys(
    keys_g, d = d, check = FALSE, fit_save = fit_save
  ),
  'argument "keys_b" is missing'
)

# Include d as keys_b
expect_error(
  bifactor.from.keys(
    keys_g, d, check = FALSE, fit_save = fit_save
  ),
  'argument "keys" is missing'
)

# Mix up keys
# Swap keys_g and keys_b
expect_error(
  bifactor.from.keys(
    keys_b, keys_g, keys, d, check = FALSE, fit_save = fit_save
  ),
  'items are in `kl_s` but they are not in `dat`'
)

# Swap keys_g and keys
expect_error(
  bifactor.from.keys(
    keys, keys_b, keys_g, d, check = FALSE, fit_save = fit_save
  ),
  '`keys_g` is not the same length as `keys_b`'
)
