# Load data
d <- BFIGritHopeImp

# Create keys
keys0 <- c("grit_c", "grit_p", "hope_a", "hope_p")
keys <- sapply(keys0, function(x) names(d)[grep(x, names(d))])

# Test normal behaviour
fit_save <- FALSE
cfa_fit <- cfa.from.keys(keys, d, check = FALSE, fit_save = fit_save)
expect_equal(length(cfa_fit), 2)
expect_equal(length(cfa_fit$fit), length(keys))
expect_equal(length(cfa_fit$par), length(keys))
expect_equal(sum(sapply(cfa_fit$fit, function(x) class(x) != "lavaan")), 0)

fit_save <- TRUE
cfa_fit <- cfa.from.keys(keys, d, check = FALSE, fit_save = fit_save)
expect_equal(length(cfa_fit), 3)
expect_equal(length(cfa_fit$fit), length(keys))
expect_equal(length(cfa_fit$par), length(keys))
expect_equal(sum(sapply(cfa_fit$fit, function(x) class(x) != "lavaan")), 0)

# Test some abnormal behaviour
# keys not specified
expect_error(
  cfa.from.keys(keys = NULL, d, check = FALSE, fit_save = fit_save),
  "one of `kl_s` or `kl_e`"
)
expect_error(
  cfa.from.keys(d = d, check = FALSE, fit_save = fit_save),
  'argument "keys" is missing'
)

# No data provided
expect_error(
  cfa.from.keys(keys, check = FALSE, fit_save = fit_save),
  'argument "d" is missing'
)

# Incorrect fit measures: All
fit_measures <- c("NotAFitMeasure", "AlsoNotAFitMeasure")
expect_warning(
  cfa.from.keys(
    keys, d, check = FALSE, fit_save = fit_save, fit_measures = fit_measures
  ),
  "All the fit measures"
)

# Incorrect fit measures: All but 1
fit_measures <- c("NotAFitMeasure", "AlsoNotAFitMeasure", "cfi")
expect_warning(
  cfa.from.keys(
    keys, d, check = FALSE, fit_save = fit_save, fit_measures = fit_measures
  ),
  "NotA|AlsoN"
)

# Mistake in keys
fit_save <- FALSE
keys_mistake <- keys
keys_mistake$grit_c[1] <- "mistake"
expect_error(
  cfa.from.keys(keys_mistake, d, check = FALSE, fit_save = fit_save),
  "items are in `kl_s` but they are not in `dat`"
)

# Non-list keys list
expect_error(
  cfa.from.keys(keys$grit_c, d, check = FALSE, fit_save = fit_save),
  "`kl_s` is not a list"
)

# Length 2 key
keys_l2 <- keys
keys_l2$grit_c <- keys$grit_c[1:2]
expect_warning(
  cfa.from.keys(keys_l2, d, check = FALSE, fit_save = fit_save),
  "only length 2"
)

# Length 1 key
keys_l1 <- keys
keys_l1$grit_c <- keys$grit_c[1]
expect_error(
  cfa.from.keys(keys_l1, d, check = FALSE, fit_save = fit_save),
  "only length 1"
)

# Unrecognised missing specification
miss <- "banana"
expect_error(
  cfa.from.keys(keys, d, check = FALSE, fit_save = fit_save, miss = miss),
  "invalid value in missing"
)

# Strange name specifications (only a few ways to stuff this up I think)
# This can stand in for all similar tests that are pretty straight forward as
# they're tested at the top of the sem.check function.
name <- 42
expect_error(
  cfa.from.keys(keys, d, check = FALSE, fit_save = fit_save, name = name),
  "`name` is not a character"
)
name <- NULL
expect_error(
  cfa.from.keys(keys, d, check = FALSE, fit_save = fit_save, name = name),
  "`name` is not a character"
)

# Data is a 0 row data.frame
d_error <- d[NULL, ]
expect_error(
  cfa.from.keys(keys, d_error, check = FALSE, fit_save = fit_save),
  "some variables have no values"
)

# Data is a string
d_string <- "cfa"
expect_error(
  cfa.from.keys(keys, d_string, check = FALSE, fit_save = fit_save),
  "items are in `kl_s` but they are not in `dat`"
)
