# Create keys
keys0 <- c("grit_c", "grit_p", "hope_p")
keys <- sapply(
  keys0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
)
keys_g0 <- c("grit", "hope")
keys_g <- sapply(
  keys_g0, function(x) names(BFIGritHope)[grep(x, names(BFIGritHope))]
)
keys_b <- sapply(keys_g0, function(x) keys0[grep(x, keys0)], simplify = FALSE)

test_that(
  "Test normal behaviour with fit_save = FALSE",
  {
    bif_fit <- bifactor.from.keys(
      keys_g, keys_b, keys, BFIGritHope, check = FALSE, fit_save = FALSE
    )
    expect_equal(length(bif_fit), 2)
    expect_equal(length(bif_fit$fit), length(keys_g))
    expect_equal(length(bif_fit$par), length(keys_g))
    expect_equal(sum(sapply(bif_fit$fit, function(x) class(x) != "lavaan")), 0)
  }
)
test_that(
  "Test normal behaviour with fit_save = TRUE",
  {
    bif_fit <- bifactor.from.keys(
      keys_g, keys_b, keys, BFIGritHope, check = FALSE, fit_save = TRUE
    )
    expect_equal(length(bif_fit), 3)
    expect_equal(length(bif_fit$fit), length(keys_g))
    expect_equal(length(bif_fit$par), length(keys_g))
    expect_equal(sum(sapply(bif_fit$fit, function(x) class(x) != "lavaan")), 0)
  }
)
test_that(
  "Mistakes with keys specification: Include CFA keys as keys",
  {
    expect_error(
      bifactor.from.keys(
        keys = keys_g, d = BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      'argument "keys_g" is missing'
    )
  }
)
test_that(
  "Mistakes with keys specification: Include CFA keys as keys_g",
  {
    expect_error(
      bifactor.from.keys(
        keys_g, d = BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      'argument "keys_b" is missing'
    )
  }
)
test_that(
  "Mistakes with keys specification: Include d as keys_b",
  {
    expect_error(
      bifactor.from.keys(
        keys_g, BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      '"keys" is missing'
    )
  }
)
test_that(
  "Mistakes with keys specification: Swap keys_g and keys_b",
  {
    expect_error(
      bifactor.from.keys(
        keys_b, keys_g, keys, BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      'group factor\\(s\\) in `keys_b` are not in `keys`'
    )
  }
)
test_that(
  "Mistakes with keys specification: Swap keys_g and keys",
  {
    expect_error(
      bifactor.from.keys(
        keys, keys_b, keys_g, BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      '`keys_g` is not the same length as `keys_b`'
    )
  }
)
test_that(
  "Mistakes in keys lists",
  {
    keys_mistake <- keys
    keys_mistake$grit_c[1] <- "mistake"
    keys_g_mistake <- keys_g
    keys_g_mistake$grit[1] <- "mistake"
    keys_b_mistake <- keys_b
    keys_b_mistake$grit[1] <- "mistake"
    expect_error(
      bifactor.from.keys(
        keys_g, keys_b, keys_mistake, BFIGritHope,
        check = FALSE, fit_save = FALSE
      ),
      "items are in `kl_s` but they are not in `dat`"
    )
    expect_error(
      bifactor.from.keys(
        keys_g_mistake, keys_b, keys, BFIGritHope,
        check = FALSE, fit_save = FALSE
      ),
      "items are in `kl_s` but they are not in `dat`"
    )
    expect_error(
      bifactor.from.keys(
        keys_g, keys_b_mistake, keys, BFIGritHope,
        check = FALSE, fit_save = FALSE
      ),
      "group factor\\(s\\) in `keys_b` are not in `keys`"
    )
  }
)
test_that(
  "Non-list keys list",
  {
    expect_error(
      bifactor.from.keys(
        keys_g, keys_b, keys$grit_c, BFIGritHope,
        check = FALSE, fit_save = FALSE
      ),
      "`keys` is not a list"
    )
    expect_error(
      bifactor.from.keys(
        keys_g$grit, keys_b, keys, BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      "`keys_g` is not a list"
    )
    expect_error(
      bifactor.from.keys(
        keys_g, keys_b$grit, keys, BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      "`keys_b` is not a list"
    )
  }
)
test_that(
  "No key for group factor",
  {
    keys_missing <- keys[-1]
    expect_error(
      bifactor.from.keys(
        keys_g, keys_b, keys_missing, BFIGritHope,
        check = FALSE, fit_save = FALSE
      ),
      "group factor\\(s\\) in `keys_b` are not in `keys`"
    )
  }
)
test_that(
  "Group factors from a different scale",
  {
    keys_b_wrong <- keys_b
    keys_b_wrong$grit <- keys_b$hope
    expect_error(
      bifactor.from.keys(
        keys_g, keys_b_wrong, keys, BFIGritHope,
        check = FALSE, fit_save = FALSE
      ),
      "items in the.*group factor are not in the.*general factor"
    )
  }
)
test_that(
  "Item in a group factor but not in the general factor",
  {
    keys_g2 <- keys_g
    keys_g2$grit <- keys_g2$grit[-1]
    expect_warning(
      bifactor.from.keys(
        keys_g2, keys_b, keys, BFIGritHope, check = FALSE, fit_save = FALSE
      ),
      "item\\(s\\) are in a group factor but not in the general factor"
    )
  }
)
